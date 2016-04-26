package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JInt, JObject}

case class MaxItems(value: Int) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("maxItems" -> JInt(value))), "(Maximum number of items: " + value + ")")
}