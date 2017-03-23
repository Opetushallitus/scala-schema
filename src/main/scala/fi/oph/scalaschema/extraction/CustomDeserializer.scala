package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ExtractionContext, Metadata, SchemaWithClassName}
import org.json4s._

trait CustomDeserializer {
  def isApplicable(schema: SchemaWithClassName): Boolean
  def extract(json: JValue, schema: SchemaWithClassName, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any]
}
