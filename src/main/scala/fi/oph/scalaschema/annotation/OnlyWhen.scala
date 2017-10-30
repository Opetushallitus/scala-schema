package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import fi.oph.scalaschema.extraction.AnyToJson
import org.json4s.{JValue, JsonAST}

case class OnlyWhen(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
  def serializableForm = SerializableOnlyWhen(path, AnyToJson.anyToJValue(value))
}

protected [scalaschema] case class SerializableOnlyWhen(path: String, value: JValue)