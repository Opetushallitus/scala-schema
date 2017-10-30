package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, Serializer}
import org.json4s.{Extraction, JValue, JsonAST}

case class OnlyWhen(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
  def serializableForm = SerializableOnlyWhen(path, OnlyWhen.anyToJValue(value))
}

protected [scalaschema] case class SerializableOnlyWhen(path: String, value: JValue)

protected [scalaschema] object OnlyWhen {
  def anyToJValue(x: Any) = {
    import Serializer.format
    Extraction.decompose(x)
  }
}