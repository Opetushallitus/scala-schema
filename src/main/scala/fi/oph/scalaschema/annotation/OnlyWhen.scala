package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST

case class OnlyWhen(path: String, value: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Only when $path = $value)")
}
