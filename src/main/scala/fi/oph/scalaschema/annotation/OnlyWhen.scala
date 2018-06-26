package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, Serializer}
import org.json4s.JsonAST.{JNull, JString}
import org.json4s.{JValue, JsonAST}

case class OnlyWhenAll(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
  def serializableForm = SerializableOnlyWhen(path, AnyToJson.anyToJValue(value))
}

case class OnlyWhen(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
  def serializableForm = SerializableOnlyWhen(path, AnyToJson.anyToJValue(value))
}

case class SerializableOnlyWhen(path: String, value: JValue)

private object AnyToJson {
  // Use sparingly! Only supports numbers, strings, booleans and Option
  def anyToJValue(x: Any): JValue = x match {
    case None => JNull
    case Some(x) => anyToJValue(x)
    case s: String => JString(s)
    case n: Number => Serializer.serializeNumber(n)
    case b: Boolean => Serializer.serializeBoolean(b)
    case _ => throw new IllegalArgumentException("Type not supported here: " + x.getClass)
  }
}