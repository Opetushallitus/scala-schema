package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ExtractionContext, JsonCursor, Metadata, SchemaWithClassName}

trait CustomDeserializer {
  def isApplicable(schema: SchemaWithClassName): Boolean
  def extract(json: JsonCursor, schema: SchemaWithClassName, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any]
}
