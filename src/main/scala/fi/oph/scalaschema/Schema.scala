package fi.oph.scalaschema

sealed trait Schema {
  def metadata: List[Metadata] = Nil
  def mapItems(f: ElementSchema => ElementSchema): Schema
}

case class OptionalSchema(itemSchema: Schema) extends Schema {
  def mapItems(f: ElementSchema => ElementSchema) = OptionalSchema(itemSchema.mapItems(f))
}
case class ListSchema(itemSchema: Schema) extends Schema {
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
case class ClassSchema(fullClassName: String, properties: List[Property], override val metadata: List[Metadata], definitions: List[SchemaWithClassName] = Nil)
                       extends ElementSchema with SchemaWithClassName with ObjectWithMetadata[ClassSchema] {
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
}
case class ClassRefSchema(fullClassName: String, override val metadata: List[Metadata]) extends ElementSchema with SchemaWithClassName with ObjectWithMetadata[ClassRefSchema] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
}
case class AnyOfSchema(alternatives: List[SchemaWithClassName], fullClassName: String) extends ElementSchema with SchemaWithClassName

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

  private def simpleClassName = {
    fullClassName.split("\\.").toList.last
  }
}

case class Property(key: String, schema: Schema, metadata: List[Metadata]) extends ObjectWithMetadata[Property] {
  def replaceMetadata(metadata: List[Metadata]) = copy(metadata = metadata)
}