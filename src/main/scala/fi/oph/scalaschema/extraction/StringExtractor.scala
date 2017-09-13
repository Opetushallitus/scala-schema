package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.RegularExpression
import fi.oph.scalaschema.{ExtractionContext, Metadata, Serializer, StringSchema}
import org.json4s.JsonAST.JString
import org.json4s._

object StringExtractor extends ExtractorWithDefaultValueSupport[String, StringSchema]{
  def extractExisting(json: JValue, schema: StringSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], String] = json match {
    case JString(stringValue) =>
      stringValue match {
        case "" if context.validate => Left(List(ValidationError(context.path, json, EmptyString())))
        case _ =>
          val errors = context.ifValidating((schema.metadata ++ metadata).collect {
            case RegularExpression(r) if !stringValue.matches(r) => ValidationError(context.path, json, RegExMismatch(r))
          }) ++ {
            EnumValues.verifyEnumValue[String](schema.enumValues, stringValue, Serializer.serializeString).left.getOrElse(Nil)
          }
          errors match {
            case Nil => Right(stringValue)
            case _ => Left(errors)
          }
      }
    case _ => Left(List(ValidationError(context.path, json, UnexpectedType("string"))))
  }
}
