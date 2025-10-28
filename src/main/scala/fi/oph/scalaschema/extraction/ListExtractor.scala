package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{MaxItems, MinItems, DeserializeSingleValueAsArray}
import fi.oph.scalaschema._
import org.json4s.JsonAST.JArray
import org.json4s._

object ListExtractor {
  def extractList(cursor: JsonCursor, ls: ListSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = cursor.json match {
    case a@JArray(values) =>
      extractListFromValues(values, cursor, ls, metadata)
    case value:JValue if metadata.exists(_.isInstanceOf[DeserializeSingleValueAsArray]) =>
      extractListFromValues(List(value), cursor, ls, metadata)
    case _ => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("array"))))
  }

  private def extractListFromValues(values: List[JValue], cursor: JsonCursor, ls: ListSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], List[Any]] = {
    val valueResults: List[Either[List[ValidationError], Any]] = values.zipWithIndex.map {
      case (itemJson, index) =>
        SchemaValidatingExtractor.extract(cursor.subCursor(itemJson, index.toString), ls.itemSchema, metadata)
    }.filter(_.isRight || !context.ignoreNonValidatingListItems)

    val metadataValidationErrors: List[ValidationError] = context.ifValidating((ls.metadata ++ metadata).collect {
      case MinItems(minItems) if values.length < minItems => ValidationError(cursor.path, cursor.json, LessThanMinimumNumberOfItems(minItems))
      case MaxItems(maxItems) if values.length > maxItems => ValidationError(cursor.path, cursor.json, MoreThanMaximumNumberOfItems(maxItems))
    })

    val errors: List[ValidationError] = valueResults.collect { case Left(errors) => errors }.flatten ++ metadataValidationErrors

    errors match {
      case Nil => Right(valueResults.collect { case Right(value) => value })
      case _ => Left(errors)
    }
  }
}
