package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.JObject

/*
    Title for object. Maps into "title" property in JSON schema
 */
case class Title(text: String) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = obj
}