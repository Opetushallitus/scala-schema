package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JDouble, JObject}

case class MaxValue(value: Double) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("maximum" -> JDouble(value))), "(Maximum value: " + value + ")")
}