package fi.oph.scalaschema

import fi.oph.scalaschema.extraction.AnyOfExtractor.CriteriaCollection
import fi.oph.scalaschema.extraction.{CustomDeserializer, ValidationError}

/**
  * Context for extraction/validation. Just initialize with a schema factory and you're good to go. You can optionally supply some CustomSerializers too.
  *
  * The context with cache some details for you, so you should retain it for later use. The caching is thread-safe, of course.
  */
case class ExtractionContext(schemaFactory: SchemaFactory, path: String = "", customDeserializers: List[CustomDeserializer] = Nil, validate: Boolean = true, criteriaCache: collection.mutable.Map[String, CriteriaCollection] = collection.mutable.Map.empty) {
  def hasSerializerFor(schema: SchemaWithClassName) = customSerializerFor(schema).isDefined
  def customSerializerFor(schema: SchemaWithClassName) = customDeserializers.find(_.isApplicable(schema))
  def ifValidating(errors: => List[ValidationError]) = if (validate) { errors } else { Nil }
  def subPath(elem: String) = path match {
    case "" => elem
    case _ => path + "." + elem
  }
  def subContext(pathElem: String) = copy(path = subPath(pathElem))
}
