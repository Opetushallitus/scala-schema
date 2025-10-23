package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.{JObject}

case class SkipSerialization() extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = obj
}
