package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.Metadata
import org.json4s.JsonAST.JObject

/**
 * Used to mark a list field to accept also a single value.
 */
class DeserializeSingleValueAsArray extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) =
    appendToDescription(obj, s"(when deserializing also accepts a single value)")
}
