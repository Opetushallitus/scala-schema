package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JBool, JDouble, JObject}

case class MinValue(value: Double) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("minimum" -> JDouble(value))), "(Minimum value: " + value + ")")
}

case class MinValueExclusive(value: Double) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj.merge(JObject("minimum" -> JDouble(value), "exclusiveMinimum" -> JBool(true))), "(Minimum value: " + value + " exclusive)")
}