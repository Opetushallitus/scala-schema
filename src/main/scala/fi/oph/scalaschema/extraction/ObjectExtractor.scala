package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ClassSchema, ExtractionContext, Metadata, SchemaValidatingExtractor}
import org.json4s.JsonAST.JObject
import org.json4s._

object ObjectExtractor {
  def extractObject(json: JValue, schema: ClassSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    json match {
      case JObject(values) =>
        val propertyResults: List[Either[List[ValidationError], Any]] = schema.properties.map { property =>
          val subContext = context.subContext(property.key)
          val jsonValue = json \ property.key
          SchemaValidatingExtractor.extract(jsonValue, property.schema, property.metadata)(subContext)
        }
        val unwantedProperties = values
          .filterNot(pair => schema.properties.find(_.key == pair._1).isDefined)
          .map(pair => ValidationError(context.subPath(pair._1), pair._2, UnwantedProperty()))
        val errors: List[ValidationError] = propertyResults.collect { case Left(errors) => errors }.flatten ++ unwantedProperties
        errors match {
          case Nil =>
            val constructor = Class.forName(schema.fullClassName).getConstructors.apply(0)
            val constructorParamsAndTypes: List[(Object, Class[_])] = propertyResults.map(_.right.get).asInstanceOf[List[Object]].zip(constructor.getParameterTypes)
            val constructorParams = constructorParamsAndTypes.map {
              case (number: Number, klass) =>
                convertNumber(number, klass)
              case (value, klass) => value
            }.asInstanceOf[List[Object]]

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

  private def convertNumber(number: Number, klass: Class[_]) =  {
    if (klass == classOf[Int] || klass == classOf[Integer]) {
      number.intValue()
    } else if (klass == classOf[Float]) {
      number.floatValue()
    } else if (klass == classOf[Double]) {
      number.doubleValue()
    } else {
      //println(s"Number conversion from ${number.getClass.getName} to ${klass.getName}")
      number
    }
  }
}
