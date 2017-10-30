package fi.oph.scalaschema.extraction

import java.sql.Timestamp
import java.time.format.DateTimeParseException
import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import fi.oph.scalaschema.{DateSchema, ExtractionContext, JsonCursor, Metadata}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.JString

object DateExtractor {
  def extractDate(cursor: JsonCursor, ds: DateSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] =
    cursor.json match {
      case JString(dateString) => try {
        Right(parse(dateString, ds.dateType))
      } catch {
        case e: DateTimeParseException => Left(List(ValidationError(cursor.path, cursor.json, DateFormatMismatch())))
      }
      case _ => Left(List(ValidationError(cursor.path, cursor.json, DateFormatMismatch())))
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
