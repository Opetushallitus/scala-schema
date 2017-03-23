package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.ExtractionContext

object EnumValues {
  def verifyEnumValue[T <: Any](enumValues: Option[List[Any]], actualValue: T)(implicit context: ExtractionContext): Either[List[ValidationError], T] = {
    enumValues match {
      case Some(values) if !values.contains(actualValue) => Left(List(ValidationError(context.path, Json.toJValue(actualValue), EnumValueMismatch(values))))
      case _ => Right(actualValue)
    }
  }
}
