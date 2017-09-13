package fi.oph.scalaschema.extraction

import java.sql.Timestamp
import java.time.format.DateTimeParseException
import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import fi.oph.scalaschema.{DateSchema, ExtractionContext, Metadata}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.JString
import org.json4s._

object DateExtractor {
  def extractDate(json: JValue, schema: DateSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] =
    json match {
      case JString(dateString) => try {
        Right(parse(dateString, schema.dateType))
      } catch {
        case e: DateTimeParseException => Left(List(ValidationError(context.path, json, DateFormatMismatch())))
      }
      case _ => Left(List(ValidationError(context.path, json, DateFormatMismatch())))
    }

  private def parse(dateString: String, dateType: Class[_]) = {
    if (dateType == classOf[LocalDate]) {
      LocalDate.parse(dateString)
    } else if (dateType == classOf[ZonedDateTime]) {
      ZonedDateTime.parse(dateString)
    } else if (dateType == classOf[Timestamp]) {
      Timestamp.from(java.time.ZonedDateTime.parse(dateString).toInstant)
    } else if (dateType == classOf[Date]) {
      Date.from(java.time.ZonedDateTime.parse(dateString).toInstant)
    } else if (dateType == classOf[DateTime]) {
      ISODateTimeFormat.dateTimeParser.withZoneUTC.parseDateTime(dateString)
    } else {
      throw new UnsupportedOperationException("Unrecognized Date type: " + dateType.getName)
    }
  }
}
