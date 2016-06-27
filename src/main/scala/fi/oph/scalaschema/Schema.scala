package fi.oph.scalaschema

import org.json4s.JsonAST.JValue

sealed trait Schema {
  def metadata: List[Metadata] = Nil
  def mapItems(f: ElementSchema => ElementSchema): Schema
  def toJson: JValue = SchemaToJson.toJsonSchema(this)
}

case class OptionalSchema(itemSchema: Schema) extends Schema {
  override def metadata: List[Metadata] = itemSchema.metadata
  def mapItems(f: ElementSchema => ElementSchema) = OptionalSchema(itemSchema.mapItems(f))
}

case class ListSchema(itemSchema: Schema) extends Schema {
  override def metadata: List[Metadata] = itemSchema.metadata
  def mapItems(f: ElementSchema => ElementSchema) = ListSchema(itemSchema.mapItems(f))
}

// Marker trait for schemas of actual elements (not optional/list wrappers)
trait ElementSchema extends Schema {
  def mapItems(f: ElementSchema => ElementSchema): Schema = f(this)
}

case class DateSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class StringSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class BooleanSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class NumberSchema(enumValues: Option[List[Any]] = None) extends ElementSchema
case class ClassSchema(fullClassName: String, properties: List[Property], override val metadata: List[Metadata] = Nil, definitions: List[SchemaWithClassName] = Nil)
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
    def collectDefinitions(schema: Schema): (Schema, List[SchemaWithClassName]) = schema match {
      case s@AnyOfSchema(alternatives, _, _, _) =>
        val collected: List[(Schema, List[SchemaWithClassName])] = alternatives.map { alt: SchemaWithClassName => collectDefinitions(alt)}
        (s.copy(alternatives = collected.map(_._1.asInstanceOf[SchemaWithClassName])), collected.flatMap(_._2))
      case s: ClassSchema =>
        val collectedProperties = s.properties.map { property =>
          val (propertySchema, defs) = collectDefinitions(property.schema)
          (property.copy(schema = propertySchema), defs)
        }
        val defsRemoved: ClassSchema = s.copy(properties = collectedProperties.map(_._1), definitions = Nil)
        val collectedDefinitions = s.definitions.flatMap { definitionSchema =>
          val (defschema2, defs) = collectDefinitions(definitionSchema)
          defschema2.asInstanceOf[SchemaWithClassName] :: defs
        }

        (defsRemoved, collectedDefinitions ++ collectedProperties.flatMap(_._2))
      case s: ElementSchema =>
        (schema, Nil)
      case s: OptionalSchema =>
        val (itemSchema, defs) = collectDefinitions(s.itemSchema)
        (OptionalSchema(itemSchema), defs)
      case s: ListSchema =>
        val (itemSchema, defs) = collectDefinitions(s.itemSchema)
        (ListSchema(itemSchema), defs)
    }
    val (mainSchema, allDefinitions) = collectDefinitions(this)
    copy(definitions = allDefinitions)
  }
}

case class ClassRefSchema(fullClassName: String, override val metadata: List[Metadata]) extends ElementSchema with SchemaWithClassName with ObjectWithMetadata[ClassRefSchema] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
  override def resolve(factory: SchemaFactory): SchemaWithClassName = factory.createSchema(fullClassName)
}
case class AnyOfSchema(alternatives: List[SchemaWithClassName], fullClassName: String, override val metadata: List[Metadata], definitions: List[SchemaWithClassName] = Nil) extends ElementSchema with SchemaWithDefinitions with ObjectWithMetadata[AnyOfSchema] {
  def withDefinitions(definitions: List[SchemaWithClassName]) = this.copy(definitions = definitions)
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
}

trait SchemaWithDefinitions extends SchemaWithClassName {
  def definitions: List[SchemaWithClassName]
  def withDefinitions(definitions: List[SchemaWithClassName]): SchemaWithDefinitions
}

trait SchemaWithClassName extends Schema {
  def fullClassName: String
  def simpleName: String = {
    simpleClassName.toLowerCase
  }
  def titleName: String = {
    simpleClassName.split("(?=\\p{Lu})").map(_.toLowerCase).mkString(" ").capitalize
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