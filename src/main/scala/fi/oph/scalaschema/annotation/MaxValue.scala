package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{MetadataSupport, Metadata}
import org.json4s.JsonAST.{JDouble, JObject}

object MaxValue extends MetadataSupport[MaxValue] {
  override def metadataClass = classOf[MaxValue]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: MaxValue) = appendToDescription(obj.merge(JObject("maximum" -> JDouble(metadata.value))), "(Maximum value: " + metadata.value + ")")
}

case class MaxValue(value: Double) extends Metadata