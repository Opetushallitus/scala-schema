package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.SchemaWithClassName
import fi.oph.scalaschema.extraction.AnyOfExtractor.CriteriaCollection
import org.json4s._
import org.json4s.jackson.JsonMethods

case class DeserializationException(path: String, message: String, cause: Exception) extends RuntimeException(s"Deserialization error at ${path} ${message}: ${cause.getMessage}", cause)

case class TooManyMatchingCasesException(path: String, cases: List[(SchemaWithClassName, CriteriaCollection)], json: JValue) extends RuntimeException(s"Deserialization error at ${path}: more than 1 matching case: \n${cases.map {
  case (schema, criteria) => s"${schema.simpleName} (${criteria.criteria.mkString(", ")})"
}.mkString("\n")}\n\ndata=${JsonMethods.pretty(json)}")

case class SchemaNotFoundException(path: String, className: String) extends RuntimeException(s"Deserialization error at ${path}: schema not found for class ${className}")

case class PathNotValidException(message: String) extends RuntimeException(message)