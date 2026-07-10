package fi.oph.scalaschema

import fi.oph.scalaschema.annotation.{EnumValue, ReadFlattened, Title}
import org.json4s.JsonAST.JValue

import java.util.Locale

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

// for Map[String, _]
case class MapSchema(itemSchema: Schema) extends Schema {
  override def metadata: List[Metadata] = itemSchema.metadata
  def mapItems(f: ElementSchema => ElementSchema) = MapSchema(itemSchema.mapItems(f))
  def collectDefinitions = {
    val (itemSchema, defs) = this.itemSchema.collectDefinitions
    (MapSchema(itemSchema), defs)
  }
}

// Marker trait for schemas of actual elements (not optional/list wrappers)
sealed trait ElementSchema extends Schema {
  def mapItems(f: ElementSchema => ElementSchema): Schema = f(this)
  def collectDefinitions: (Schema, List[SchemaWithClassName]) = (this, Nil)
}

sealed trait SimpleSchema extends ElementSchema
case class DateSchema(dateType: Class[_]) extends SimpleSchema
case class StringSchema(enumValues: Option[List[String]] = None) extends SimpleSchema
case class BooleanSchema(enumValues: Option[List[Boolean]] = None) extends SimpleSchema
case class NumberSchema(numberType: Class[_], enumValues: Option[List[Number]] = None) extends SimpleSchema
case class DefinitionKey(fullClassName: String, variantQualifier: Option[List[String]] = None) {
  def refValue: String =
    (variantQualifier.getOrElse(Nil) :+ DefinitionKey.simpleName(fullClassName))
      .map(DefinitionKey.sanitizeForRef)
      .mkString(DefinitionKey.RefSeparator)
}

object DefinitionKey {
  private val RefSeparator = ":"

  // JSON Schema definition ref parts support Unicode letters, decimal digits and underscores.
  // Other characters are normalized to underscores.
  private val UnsupportedRefCharacter = """[^\p{L}\p{Nd}_]""".r

  private def simpleName(fullClassName: String): String =
    fullClassName.split("\\.").toList.last

  private def sanitizeForRef(part: String): String =
    UnsupportedRefCharacter.replaceAllIn(part.toLowerCase(Locale.ROOT), "_")
}

case class ClassSchema(
  override val definitionKey: DefinitionKey,
  properties: List[Property],
  override val metadata: List[Metadata] = Nil,
  definitions: List[SchemaWithClassName] = Nil,
  specialized: Boolean = false,
  readFlattened: Option[FlattenedSchema] = None
)
                       extends ElementSchema with SchemaWithDefinitions with ObjectWithMetadata[ClassSchema] {
  def getPropertyValue(property: Property, target: AnyRef): AnyRef = {
    val keyWithScalaNameEncoding = scala.reflect.NameTransformer.encode(property.key)
    target.getClass.getMethod(keyWithScalaNameEncoding).invoke(target)
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

  override def resolve(factory: SchemaFactory): SchemaWithClassName = this

  lazy val asAnyOfSchema = AnyOfSchema(definitionKey, this.copy(readFlattened = None) :: readFlattened.toList)
}

case class ClassRefSchema(override val definitionKey: DefinitionKey, override val metadata: List[Metadata] = Nil) extends ElementSchema with SchemaWithClassName with ObjectWithMetadata[ClassRefSchema] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)

  @deprecated("Root-blind ClassRefSchema resolution can lose root-specific schema variants. Use resolve(factory, rootSchema) when resolving a ref from a root schema.", "2.44.0_2.13")
  def resolve(factory: SchemaFactory): SchemaWithClassName = factory.createSchemaWithoutRootSchema(this)

  override def resolve(factory: SchemaFactory, rootSchema: Schema): SchemaWithClassName = factory.createSchema(this, rootSchema)
}

case class AnyOfSchema(
  override val definitionKey: DefinitionKey,
  alternatives: List[SchemaWithClassName],
  override val metadata: List[Metadata] = Nil,
  definitions: List[SchemaWithClassName] = Nil
) extends ElementSchema with SchemaWithDefinitions with ObjectWithMetadata[AnyOfSchema] {
  if (alternatives.isEmpty) throw new RuntimeException("AnyOfSchema needs at least one alternative")
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
  def findAlternative(obj: Any): Option[SchemaWithClassName] = {
    alternatives.find { classType =>
      classType.fullClassName == obj.getClass.getName
    }
  }

  override def resolve(factory: SchemaFactory): SchemaWithClassName = this
}
case class FlattenedSchema(classSchema: ClassSchema, property: Property) extends SchemaWithClassName with ElementSchema {
  override def collectDefinitions: (Schema, List[SchemaWithClassName]) = {
    val (newItemSchema, defs) = property.schema.collectDefinitions
    (this.copy(property = property.copy(schema = newItemSchema)), defs)
  }

  def getValue(target: AnyRef): AnyRef = {
    classSchema.getPropertyValue(property, target)
  }

  override def definitionKey: DefinitionKey = classSchema.definitionKey

  override def resolve(factory: SchemaFactory): SchemaWithClassName = this
}

sealed trait SchemaWithDefinitions extends SchemaWithClassName {
  def definitions: List[SchemaWithClassName]
  def withDefinitions(definitions: List[SchemaWithClassName]): SchemaWithDefinitions
  def moveDefinitionsToTopLevel: SchemaWithDefinitions

  // A ClassRefSchema can be resolved from the current root schema before falling back to
  // SchemaFactory. When a class ref appears inside a root schema, the referenced class
  // schema may already have been created while scanning that root. The root schema's
  // definitions contain schemas found through the root's fields, their fields, and so on.
  //
  // This matters for root-specific schema features such as @IncludeComputedProperty.
  // During the root scan, child schema creation receives the same ScanState, so the child
  // schemas in definitions preserve the root-specific included computed property
  // configuration. Resolving the class ref directly through SchemaFactory would use the
  // referenced class as its own root schema and lose that configuration.
  private[scalaschema] def findSchemaForClassRef(classRef: ClassRefSchema): Option[SchemaWithClassName] = {
    if (definitionKey == classRef.definitionKey) {
      Some(this)
    } else {
      definitions.find(_.definitionKey == classRef.definitionKey)
    }
  }

  protected [scalaschema] def definitionsCollectedFromDefinitions: List[SchemaWithClassName] = this.definitions.flatMap { definitionSchema =>
    val (defschema2, defs) = definitionSchema.collectDefinitions
    defschema2.asInstanceOf[SchemaWithClassName] :: defs
  }
}

sealed trait SchemaWithClassName extends Schema {
  def definitionKey: DefinitionKey
  def fullClassName: String = definitionKey.fullClassName
  def definitionName: String = definitionKey.refValue
  def simpleName: String = {
    simpleClassName.toLowerCase
  }
  def title: String = {
    this.metadata.collect{case Title(t) => t} match {
      case Nil =>
        simpleClassName.split("(?=\\p{Lu})").map(_.toLowerCase).mkString(" ").replaceAll("_ ", "-").capitalize
      case titles =>
        titles.mkString(" ")
    }
  }
  private def simpleClassName = {
    fullClassName.split("\\.").toList.last
  }

  def appliesToClass(k: Class[_]) = k.getName == fullClassName

  def resolve(factory: SchemaFactory): SchemaWithClassName
  def resolve(factory: SchemaFactory, rootSchema: Schema): SchemaWithClassName = resolve(factory)
}

case class Property(key: String, schema: Schema, metadata: List[Metadata] = Nil, synthetic: Boolean = false, computed: Boolean = false) extends ObjectWithMetadata[Property] {
  def replaceMetadata(metadata: List[Metadata]) =
    copy(
      metadata = metadata,
      schema = EnumValue.addEnumValues(schema, metadata.collect({ case EnumValue(v) => v }))
    )

  def title = metadata.flatMap {
    case Title(t) => Some(t)
    case _ => None
  }.headOption.getOrElse(key.split("(?=\\p{Lu})").map(_.toLowerCase).mkString(" ").replaceAll("_ ", "-").capitalize)
}

case class AnySchema() extends SimpleSchema
case class AnyObjectSchema() extends SimpleSchema
case class AnyListSchema() extends SimpleSchema
