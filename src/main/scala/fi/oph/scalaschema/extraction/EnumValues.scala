package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.ExtractionContext
import org.json4s.JValue

object EnumValues {
  def verifyEnumValue[T <: Any](enumValues: Option[List[T]], actualValue: T, valueToJson: T => JValue)(implicit context: ExtractionContext): Either[List[ValidationError], T] = {
    enumValues match {
      case Some(values) if !values.contains(actualValue) => Left(List(ValidationError(context.path, valueToJson(actualValue), EnumValueMismatch(values.map(valueToJson)))))
      case _ => Right(actualValue)
    }
  }
}
