package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{MaxItems, MinItems}
import fi.oph.scalaschema.{ExtractionContext, ListSchema, Metadata, SchemaValidatingExtractor}
import org.json4s.JsonAST.JArray
import org.json4s._

object ListExtractor {
  def extractList(json: JValue, as: ListSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = json match {
    case JArray(values) =>
      val valueResults: List[Either[List[ValidationError], Any]] = values.zipWithIndex.map {
        case (itemJson, index) =>
          SchemaValidatingExtractor.extract(itemJson, as.itemSchema, metadata)(context.subContext(index.toString))
      }

      val metadataValidationErrors: List[ValidationError] = context.ifValidating((as.metadata ++ metadata).collect {
        case MinItems(minItems) if values.length < minItems => ValidationError(context.path, json, LessThanMinimumNumberOfItems(minItems))
        case MaxItems(maxItems) if values.length > maxItems => ValidationError(context.path, json, MoreThanMaximumNumberOfItems(maxItems))
      })

      val errors: List[ValidationError] = valueResults.collect { case Left(errors) => errors }.flatten ++ metadataValidationErrors

      errors match {
        case Nil => Right(valueResults.map(_.right.get))
        case _ => Left(errors)
      }
    case _ => Left(List(ValidationError(context.path, json, UnexpectedType("array"))))
  }
}
