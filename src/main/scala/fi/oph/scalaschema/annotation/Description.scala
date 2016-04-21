package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, MetadataSupport}
import org.json4s.JsonAST.JObject

object Description extends MetadataSupport[Description] {
  override def metadataClass = classOf[Description]

  override def appendMetadataToJsonSchema(obj: JObject, desc: Description) = appendToDescription(obj, desc.text)
}

case class Description(text: String) extends Metadata