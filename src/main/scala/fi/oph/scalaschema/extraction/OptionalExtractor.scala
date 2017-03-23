package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ExtractionContext, Metadata, OptionalSchema, SchemaValidatingExtractor}
import org.json4s._

object OptionalExtractor {
  def extractOptional(json: JValue, as: OptionalSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = json match {
    case JNothing => Right(None)
    case _ => SchemaValidatingExtractor.extract(json, as.itemSchema, metadata).right.map(value => Some(value))
  }
}
