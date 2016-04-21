package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{MetadataSupport, Metadata}
import org.json4s.JsonAST.{JInt, JObject}

object MinItems extends MetadataSupport[MinItems] {
  override def metadataClass = classOf[MinItems]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: MinItems) = appendToDescription(obj.merge(JObject("minItems" -> JInt(metadata.value))), "(Minimum number of items: " + metadata.value + ")")
}

case class MinItems(value: Int) extends Metadata