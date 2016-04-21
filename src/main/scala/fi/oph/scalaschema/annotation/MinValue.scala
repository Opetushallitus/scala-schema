package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{MetadataSupport, Metadata}
import org.json4s.JsonAST.{JDouble, JObject}

object MinValue extends MetadataSupport[MinValue] {
  override def metadataClass = classOf[MinValue]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: MinValue) = appendToDescription(obj.merge(JObject("minimum" -> JDouble(metadata.value))), "(Minimiarvo: " + metadata.value + ")")
}

case class MinValue(value: Double) extends Metadata
