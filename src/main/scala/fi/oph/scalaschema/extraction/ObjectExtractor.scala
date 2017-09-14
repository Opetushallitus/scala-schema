package fi.oph.scalaschema.extraction

import fi.oph.scalaschema._
import org.json4s.JsonAST.JObject
import org.json4s._

object ObjectExtractor {
  def extractObject(json: JValue, schema: ClassSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], AnyRef] = {
    json match {
      case JObject(values) =>
        val propertyResults: List[Either[List[ValidationError], Any]] = schema.properties
          .filterNot(_.synthetic)
          .map { property =>
            val subContext = context.subContext(property.key)
            val jsonValue = json \ property.key
            SchemaValidatingExtractor.extract(jsonValue, property.schema, property.metadata)(subContext, rootSchema)
          }
        val unexpectedProperties = if (context.ignoreUnexpectedProperties) Nil else values
          .filterNot(pair => schema.properties.find(_.key == pair._1).isDefined)
          .map(pair => ValidationError(context.subPath(pair._1), pair._2, UnexpectedProperty()))
        val errors: List[ValidationError] = propertyResults.collect { case Left(errors) => errors }.flatten ++ unexpectedProperties
        errors match {
          case Nil =>
            val constructor = Class.forName(schema.fullClassName).getConstructors.apply(0)
            val constructorParams: List[Object] = propertyResults.map(_.right.get).asInstanceOf[List[Object]]
            try {
              Right(constructor.newInstance(constructorParams: _*).asInstanceOf[AnyRef])
            } catch {
              case e: Exception => throw new DeserializationException(context.path, s"instantiating ${schema.fullClassName} with ${constructorParams}", e)
            }

          case _ => Left(errors)
        }
      case _ => Left(List(ValidationError(context.path, json, UnexpectedType("object"))))
    }
  }
}
