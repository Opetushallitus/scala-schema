package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.DefaultValue
import fi.oph.scalaschema.{ExtractionContext, Metadata, Schema}
import org.json4s._

trait ExtractorWithDefaultValueSupport[V <: Any, S <: Schema] {
  def extract(json: JValue, schema: S, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], V] = json match {
    case JNothing =>
      DefaultValue.getDefaultValue[V](metadata) match {
        case Some(v) => Right(v)
        case _ => Left(List(ValidationError(context.path, json, MissingProperty())))
      }
    case _ =>
      extractExisting(json, schema, metadata)
  }
  def extractExisting(json: JValue, schema: S, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], V]
}