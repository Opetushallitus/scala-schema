package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import org.json4s.JsonAST.JBool
import org.json4s._

object BooleanExtractor extends ExtractorWithDefaultValueSupport[Boolean, BooleanSchema] {
  def extractExisting(cursor: JsonCursor, bs: BooleanSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Boolean] = cursor.json match {
    case JBool(b) =>
      EnumValues.verifyEnumValue[Boolean](cursor.path, bs.enumValues, b, Serializer.serializeBoolean)
    case json =>
      Left(List(ValidationError(cursor.path, json, UnexpectedType("boolean"))))
  }
}

