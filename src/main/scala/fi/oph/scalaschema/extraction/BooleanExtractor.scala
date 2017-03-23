package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{BooleanSchema, ExtractionContext, Metadata}
import org.json4s.JsonAST.JBool
import org.json4s._

object BooleanExtractor {
  def extractBoolean(json: JValue, schema: BooleanSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Boolean] = json match {
    case JBool(b) =>
      EnumValues.verifyEnumValue[Boolean](schema.enumValues, b)
    case _ =>
      Left(List(ValidationError(context.path, json, UnexpectedType("boolean"))))
  }
}
