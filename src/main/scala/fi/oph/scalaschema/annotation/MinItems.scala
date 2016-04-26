package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JInt, JObject}

case class MinItems(value: Int) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("minItems" -> JInt(value))), "(Minimum number of items: " + value + ")")
}