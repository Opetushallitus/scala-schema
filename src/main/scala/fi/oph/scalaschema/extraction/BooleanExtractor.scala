package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{BooleanSchema, ExtractionContext, Metadata, Schema}
import org.json4s.JsonAST.JBool
import org.json4s._

object BooleanExtractor extends ExtractorWithDefaultValueSupport[Boolean, BooleanSchema] {
  def extractExisting(json: JValue, schema: BooleanSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Boolean] = json match {
    case JBool(b) =>
      EnumValues.verifyEnumValue[Boolean](schema.enumValues, b, json)
    case _ =>
      Left(List(ValidationError(context.path, json, UnexpectedType("boolean"))))
  }
}

