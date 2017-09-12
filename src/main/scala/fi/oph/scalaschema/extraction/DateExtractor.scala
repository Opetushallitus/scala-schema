package fi.oph.scalaschema.extraction

import java.time.LocalDate
import java.time.format.DateTimeParseException

import fi.oph.scalaschema.{DateSchema, ExtractionContext, Metadata}
import org.json4s.JsonAST.JString
import org.json4s._

object DateExtractor {
  def extractDate(json: JValue, schema: DateSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], LocalDate] = json match {
    case JString(dateString) => try {
      EnumValues.verifyEnumValue(schema.enumValues, LocalDate.parse(dateString), json)
    } catch {
      case e: DateTimeParseException => Left(List(ValidationError(context.path, json, DateFormatMismatch())))
    }
    case _ => Left(List(ValidationError(context.path, json, DateFormatMismatch())))
  }
}
