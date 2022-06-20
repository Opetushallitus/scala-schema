package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, ValueConversion}
import org.json4s.{JValue, JsonAST}

case class OnlyWhen(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
  def serializableForm = SerializableOnlyWhen(path, ValueConversion.anyToJValue(value))
}

case class SerializableOnlyWhen(path: String, value: JValue)