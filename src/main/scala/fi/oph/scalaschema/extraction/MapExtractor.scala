package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import org.json4s.JValue
import org.json4s.JsonAST.{JField, JObject}

object MapExtractor {
  def extractMap(cursor: JsonCursor, ms: MapSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = cursor.json match {
    case o@JObject(fields) =>
      val valueResults: List[Either[List[ValidationError], (String, Any)]] = fields.map {
        case JField(key, valueJson) =>
          SchemaValidatingExtractor.extract(cursor.subCursor(valueJson, key), ms.itemSchema, metadata).right.map(fieldValue => (key -> fieldValue))
      }

      val errors: List[ValidationError] = valueResults.collect { case Left(errors) => errors }.flatten
      errors match {
        case Nil => Right(valueResults.map(_.right.get).toMap)
        case _ => Left(errors)
      }
    case _ => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("object"))))
  }
}
