package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import org.json4s._

object OptionalExtractor {
  def extractOptional(json: JValue, as: OptionalSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], AnyRef] = json match {
    case JNothing => Right(None)
    case JNull => Right(None)
    case _ => SchemaValidatingExtractor.extract(json, as.itemSchema, metadata).right.map(value => Some(value))
  }
}
