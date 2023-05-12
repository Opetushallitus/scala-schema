package fi.oph.scalaschema

import fi.oph.scalaschema.extraction.AnyOfExtractor.DiscriminatorCriterion
import fi.oph.scalaschema.extraction.{CustomDeserializer, ValidationError}
import org.json4s.{JArray, JValue}
import org.json4s.JsonAST.JNothing

/**
  * Context for extraction/validation. Just initialize with a schema factory and you're good to go. You can optionally supply some CustomSerializers too.
  *
  * The context with cache some details for you, so you should retain it for later use. The caching is thread-safe, of course.
  */
case class ExtractionContext(schemaFactory: SchemaFactory,
                             customDeserializers: List[CustomDeserializer] = Nil,
                             validate: Boolean = true,
                             ignoreUnexpectedProperties: Boolean = false,
                             allowEmptyStrings: Boolean = true,
                             criteriaCache: collection.mutable.Map[String, DiscriminatorCriterion] = collection.mutable.Map.empty,
                             ignoreNonValidatingListItems: Boolean = false,
                             stripClassReferences: Boolean = true,
                             omitNullFromInput: Boolean = false) {
  def hasSerializerFor(schema: SchemaWithClassName) = customSerializerFor(schema).isDefined
  def customSerializerFor(schema: SchemaWithClassName) = customDeserializers.find(_.isApplicable(schema))
  def ifValidating(errors: => List[ValidationError]) = if (validate) { errors } else { Nil }
}

case class JsonCursor(json: JValue, parent: Option[JsonCursor] = None, path: String = "") {
  def subCursor(json: JValue, pathElem: String) = JsonCursor(json, Some(this), subPath(pathElem))
  def subPath(pathElem: String) = JsonCursor.subPath(path, pathElem)
  def navigate(subPath: String) = {
    subPath.split("/").foldLeft(this) {
      case (currentCursor, "..") => currentCursor.parent.getOrElse(JsonCursor(JNothing))
      case (currentCursor, index) if currentCursor.json.isInstanceOf[JArray] && index.forall(Character.isDigit) =>
        currentCursor.json.asInstanceOf[JArray].arr.lift(index.toInt)
          .map(child => currentCursor.subCursor(child, subPath))
          .getOrElse(JsonCursor(JNothing))
      case (currentCursor, pathElem) => currentCursor.subCursor(currentCursor.json \ pathElem, pathElem)
    }
  }
}

object JsonCursor {
  def subPath(path: String, pathElem: String) = path match {
    case "" => pathElem
    case _ => path + "." + pathElem
  }
}