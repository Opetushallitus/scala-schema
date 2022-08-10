package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import fi.oph.scalaschema.annotation.DefaultValue
import org.json4s._

object OptionalExtractor {
  def extractOptional(cursor: JsonCursor, schema: OptionalSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = cursor.json match {
    case JNothing | JNull =>
      DefaultValue.getDefaultValue[AnyRef](metadata) match {
        case Some(v) => Right(v)
        case _ => Right(None)
      }
    case _ => SchemaValidatingExtractor.extract(cursor, schema.itemSchema, metadata).right.map(value => Some(value))
  }
}
