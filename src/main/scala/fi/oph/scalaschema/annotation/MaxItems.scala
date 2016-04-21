package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, MetadataSupport}
import org.json4s.JsonAST.{JInt, JObject}

object MaxItems extends MetadataSupport[MaxItems] {
  override def metadataClass = classOf[MaxItems]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: MaxItems) = appendToDescription(obj.merge(JObject("maxItems" -> JInt(metadata.value))), "(Maximum number of items: " + metadata.value + ")")
}

case class MaxItems(value: Int) extends Metadata