package fi.oph.scalaschema

import fi.oph.scalaschema.annotation.Title
import org.json4s.JsonAST.JValue

sealed trait Schema {
  def metadata: List[Metadata] = Nil
  def mapItems(f: ElementSchema => ElementSchema): Schema
  def toJson: JValue = SchemaToJson.toJsonSchema(this)
  // Returns this schema with definitions removed, plus list of definitions removed
  def collectDefinitions: (Schema, List[SchemaWithClassName])
}

case class OptionalSchema(itemSchema: Schema) extends Schema {
  override def metadata: List[Metadata] = itemSchema.metadata
  def mapItems(f: ElementSchema => ElementSchema) = OptionalSchema(itemSchema.mapItems(f))
  def collectDefinitions = {
    val (itemSchema, defs) = this.itemSchema.collectDefinitions
    (OptionalSchema(itemSchema), defs)
  }
}

case class ListSchema(itemSchema: Schema) extends Schema {
  override def metadata: List[Metadata] = itemSchema.metadata
  def mapItems(f: ElementSchema => ElementSchema) = ListSchema(itemSchema.mapItems(f))
  def collectDefinitions = {
    val (itemSchema, defs) = this.itemSchema.collectDefinitions
    (ListSchema(itemSchema), defs)
  }
}

// Marker trait for schemas of actual elements (not optional/list wrappers)
trait ElementSchema extends Schema {
  def mapItems(f: ElementSchema => ElementSchema): Schema = f(this)
  def collectDefinitions: (Schema, List[SchemaWithClassName]) = (this, Nil)
}

case class DateSchema(enumValues: Option[List[Any]] = None) extends ElementSchema // Why untyped lists?
case class StringSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class BooleanSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class NumberSchema(numberType: Class[_], enumValues: Option[List[Any]] = None) extends ElementSchema
case class ClassSchema(fullClassName: String, properties: List[Property], override val metadata: List[Metadata] = Nil, definitions: List[SchemaWithClassName] = Nil, specialized: Boolean = false)
                       extends ElementSchema with SchemaWithDefinitions with ObjectWithMetadata[ClassSchema] {
  override def getSchema(className: String): Option[SchemaWithClassName] = {
    if (className == this.fullClassName) {
      Some(this)
    } else {
      definitions.find(_.fullClassName == className)
    }
  }

  def getPropertyValue(property: Property, target: AnyRef): AnyRef = {
    target.getClass.getMethod(property.key).invoke(target)
  }
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)

  def withDefinitions(definitions: List[SchemaWithClassName]) = this.copy(definitions = definitions)

  def moveDefinitionsToTopLevel: ClassSchema = {
    val (thisSchemaWithoutDefs, allDefinitions) = this.collectDefinitions
    thisSchemaWithoutDefs.asInstanceOf[ClassSchema].copy(definitions = allDefinitions)
  }

  override def collectDefinitions: (Schema, List[SchemaWithClassName]) = {
    val collectedProperties = this.properties.map { property =>
      val (propertySchema, defs) = property.schema.collectDefinitions
      (property.copy(schema = propertySchema), defs)
    }
    val propertiesWithDefsRemoved: List[Property] = collectedProperties.map(_._1)

    val definitionsCollectedFromProperties: List[SchemaWithClassName] = collectedProperties.flatMap(_._2)

    val thisSchemaWithDefinitionsRemoved: ClassSchema = this.copy(properties = propertiesWithDefsRemoved, definitions = Nil)

    (thisSchemaWithDefinitionsRemoved, (definitionsCollectedFromDefinitions ++ definitionsCollectedFromProperties).distinct)
  }
}

case class ClassRefSchema(fullClassName: String, override val metadata: List[Metadata]) extends ElementSchema with SchemaWithClassName with ObjectWithMetadata[ClassRefSchema] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
  override def resolve(factory: SchemaFactory): SchemaWithClassName = factory.createSchema(fullClassName)
}
case class AnyOfSchema(alternatives: List[SchemaWithClassName], fullClassName: String, override val metadata: List[Metadata], definitions: List[SchemaWithClassName] = Nil) extends ElementSchema with SchemaWithDefinitions with ObjectWithMetadata[AnyOfSchema] {
  def withDefinitions(definitions: List[SchemaWithClassName]) = this.copy(definitions = definitions)
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
  override def collectDefinitions: (AnyOfSchema, List[SchemaWithClassName]) = {
    val collectedFromAlternatives: List[(Schema, List[SchemaWithClassName])] = alternatives.map { alt: SchemaWithClassName => alt.collectDefinitions}
    val alternativesWithoutDefinitions: List[SchemaWithClassName] = collectedFromAlternatives.map(_._1.asInstanceOf[SchemaWithClassName])
    val definitionsCollectedFromAlternatives: List[SchemaWithClassName] = collectedFromAlternatives.flatMap(_._2)

    (this.copy(alternatives = alternativesWithoutDefinitions, definitions = Nil), definitionsCollectedFromAlternatives ++ definitionsCollectedFromDefinitions)
  }
  def moveDefinitionsToTopLevel: AnyOfSchema = {
    val (thisSchemaWithoutDefs, allDefinitions) = this.collectDefinitions
    thisSchemaWithoutDefs.withDefinitions(allDefinitions.distinct)
  }
}

trait SchemaWithDefinitions extends SchemaWithClassName {
  def definitions: List[SchemaWithClassName]
  def withDefinitions(definitions: List[SchemaWithClassName]): SchemaWithDefinitions
  def moveDefinitionsToTopLevel: SchemaWithDefinitions
  protected [scalaschema] def definitionsCollectedFromDefinitions: List[SchemaWithClassName] = this.definitions.flatMap { definitionSchema =>
    val (defschema2, defs) = definitionSchema.collectDefinitions
    defschema2.asInstanceOf[SchemaWithClassName] :: defs
  }
}

trait SchemaWithClassName extends Schema {
  def fullClassName: String
  def simpleName: String = {
    simpleClassName.toLowerCase
  }
  def titleName: String = {
    this.metadata.collect{case Title(t) => t} match {
      case Nil =>
        simpleClassName.split("(?=\\p{Lu})").map(_.toLowerCase).mkString(" ").replaceAll("_ ", "-").capitalize
      case titles =>
        titles.mkString(" ")
    }
  }
  def getSchema(className: String): Option[SchemaWithClassName] = if (className == fullClassName) {
    Some(this)
  } else {
    None
  }

  /*
    Replace ClassRefSchema with ClassSchema
   */
  def resolve(factory: SchemaFactory): SchemaWithClassName = this

  private def simpleClassName = {
    fullClassName.split("\\.").toList.last
  }

  def appliesToClass(k: Class[_]) = k.getName == fullClassName
}

case class Property(key: String, schema: Schema, metadata: List[Metadata] = Nil) extends ObjectWithMetadata[Property] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
}