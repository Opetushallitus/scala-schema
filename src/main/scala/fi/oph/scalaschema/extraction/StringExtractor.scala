package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.RegularExpression
import fi.oph.scalaschema._
import org.json4s._

object StringExtractor extends ExtractorWithDefaultValueSupport[String, StringSchema]{
  def extractExisting(cursor: JsonCursor, ss: StringSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], String] = cursor.json match {
    case JString(stringValue) => validateString(stringValue, cursor, ss, metadata)
    case v: JDouble => validateString(v.values.toString, cursor, ss, metadata)
    case v: JDecimal => validateString(v.values.toString, cursor, ss, metadata)
    case v: JLong => validateString(v.values.toString, cursor, ss, metadata)
    case v: JInt => validateString(v.values.toString, cursor, ss, metadata)
    case JBool(b) => validateString(b.toString, cursor, ss, metadata)
    case _ => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("string"))))
  }

  private def validateString(stringValue: String, cursor: JsonCursor, schema: StringSchema, metadata: List[Metadata])(implicit context: ExtractionContext) = {
    stringValue match {
      case "" if !context.allowEmptyStrings => Left(List(ValidationError(cursor.path, cursor.json, EmptyString())))
      case _ =>
        val errors = context.ifValidating((schema.metadata ++ metadata).collect {
          case RegularExpression(r) if !stringValue.matches(r) => ValidationError(cursor.path, cursor.json, RegExMismatch(r))
        }) ++ {
          EnumValues.verifyEnumValue[String](cursor.path, schema.enumValues, stringValue, Serializer.serializeString).left.getOrElse(Nil)
        }
        errors match {
          case Nil => Right(stringValue)
          case _ => Left(errors)
        }
    }
  }
}
