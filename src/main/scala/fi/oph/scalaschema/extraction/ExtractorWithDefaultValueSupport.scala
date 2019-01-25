package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.DefaultValue
import fi.oph.scalaschema.{ExtractionContext, JsonCursor, Metadata, Schema}
import org.json4s._

trait ExtractorWithDefaultValueSupport[V <: Any, S <: Schema] {
  def extract(cursor: JsonCursor, schema: S, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], V] = cursor.json match {
    case JNothing =>
      DefaultValue.getDefaultValue[V](metadata) match {
        case Some(v) => Right(v)
        case _ => Left(List(ValidationError(cursor.path, cursor.json, MissingProperty())))
      }
    case JNull =>
      DefaultValue.getDefaultValue[V](metadata) match {
        case Some(v) => Right(v)
        case _ => extractExisting(cursor, schema, metadata)
      }
    case _ =>
      extractExisting(cursor, schema, metadata)
  }
  def extractExisting(cursor: JsonCursor, schema: S, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], V]
}