package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{MetadataSupport, Metadata}
import org.json4s.JsonAST.JObject

object ReadOnly extends MetadataSupport[ReadOnly] {
  override def metadataClass = classOf[ReadOnly]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: ReadOnly) = appendToDescription(obj, metadata.desc)
}

case class ReadOnly(desc: String) extends Metadata