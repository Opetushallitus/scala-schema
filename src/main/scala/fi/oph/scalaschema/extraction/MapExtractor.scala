package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import org.json4s.JValue
import org.json4s.JsonAST.{JField, JObject}

object MapExtractor {
  def extractMap(json: JValue, ms: MapSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = json match {
    case JObject(fields) =>
      val valueResults: List[Either[List[ValidationError], (String, Any)]] = fields.map {
        case JField(key, valueJson) =>
          SchemaValidatingExtractor.extract(valueJson, ms.itemSchema, metadata).map(fieldValue => (key -> fieldValue))
      }

      val errors: List[ValidationError] = valueResults.collect { case Left(errors) => errors }.flatten
      errors match {
        case Nil => Right(valueResults.map(_.right.get).toMap)
        case _ => Left(errors)
      }
    case _ => Left(List(ValidationError(context.path, json, UnexpectedType("object"))))
  }
}
