package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JObject, JString}

case class RegularExpression(pattern: String) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("pattern" -> JString(pattern))), "(Format: " + pattern + ")")
}