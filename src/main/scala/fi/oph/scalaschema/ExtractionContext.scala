package fi.oph.scalaschema

import fi.oph.scalaschema.extraction.AnyOfExtractor.CriteriaCollection
import fi.oph.scalaschema.extraction.{CustomDeserializer, PathNotValidException, ValidationError}
import org.json4s.JValue

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
                             criteriaCache: collection.mutable.Map[String, CriteriaCollection] = collection.mutable.Map.empty) {
  def hasSerializerFor(schema: SchemaWithClassName) = customSerializerFor(schema).isDefined
  def customSerializerFor(schema: SchemaWithClassName) = customDeserializers.find(_.isApplicable(schema))
  def ifValidating(errors: => List[ValidationError]) = if (validate) { errors } else { Nil }
}

case class JsonCursor(json: JValue, parent: Option[JsonCursor] = None, path: String = "") {
  def subCursor(json: JValue, pathElem: String) = JsonCursor(json, Some(this), subPath(pathElem))
  def subPath(pathElem: String) = JsonCursor.subPath(path, pathElem)
  def navigate(subPath: String) = {
    subPath.split("/").foldLeft(this) {
      case (currentCursor, "..") => currentCursor.parent.getOrElse(throw new PathNotValidException(s"path $path not valid as a subpath of ${this.path}"))
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