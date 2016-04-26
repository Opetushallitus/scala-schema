package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JDouble, JObject}

case class MinValue(value: Double) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("minimum" -> JDouble(value))), "(Minimum value: " + value + ")")
}




