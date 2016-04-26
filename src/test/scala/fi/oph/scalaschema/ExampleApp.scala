package fi.oph.scalaschema

import fi.oph.scalaschema.annotation.{Description, MaxValue, MinValue, RegularExpression}
import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.jackson.JsonMethods

object ExampleApp extends App {
  val schema: Schema = SchemaFactory.default.createSchema(classOf[Cat])
  val schemaAsJson: JValue = schema.toJson
  val schemaAsString = JsonMethods.pretty(schemaAsJson)
  println(schemaAsString)
}

case class Cat(name: String)


object ExampleWithAnnotations extends App {
  val schema: Schema = SchemaFactory.default.createSchema(classOf[AnnotatedCat])
  val schemaAsJson: JValue = schema.toJson
  val schemaAsString = JsonMethods.pretty(schemaAsJson)
  println(schemaAsString)
}

@Description("A cat")
case class AnnotatedCat(
  @MinValue(3) @MaxValue(4)
  feet: Int,
  @RegularExpression(".*")
  name: String
)

object ExampleWithCustomAnnotations extends App {
  val annotations = classOf[ReadOnly] :: SchemaFactory.defaultAnnotations
  val schema: Schema = SchemaFactory(annotations).createSchema(classOf[AnnotatedCat])
  val schemaAsJson: JValue = schema.toJson
  val schemaAsString = JsonMethods.pretty(schemaAsJson)
  println(schemaAsString)
}

case class ReadOnly(why: String) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj, why)
}

case class ReadOnlyCat(
  @ReadOnly("Please don't mutate")
  feet: Int = 4
)
