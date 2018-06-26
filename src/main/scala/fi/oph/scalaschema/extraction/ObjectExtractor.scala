package fi.oph.scalaschema.extraction

import java.lang.reflect.Constructor

import fi.oph.scalaschema._
import fi.oph.scalaschema.annotation.{DefaultValue, OnlyWhen, OnlyWhenAll, SerializableOnlyWhen}
import org.json4s.JsonAST.JObject
import org.json4s._

object ObjectExtractor {
  def extractFlattenedObject(cursor: JsonCursor, s: FlattenedSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    SchemaValidatingExtractor.extract(cursor, s.property.schema, metadata).right.map { extractedValue =>
      val constructorParams = s.classSchema.properties.filterNot(_.synthetic).map { p => if (p == s.property) extractedValue else None }
      instantiateCaseClass(cursor.path, s.fullClassName, constructorParams)
    }
  }

  def extractObject(cursor: JsonCursor, cs: ClassSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = cs.readFlattened match {
    case Some(flattenedSchema) => AnyOfExtractor.extractAnyOf(cursor, cs.asAnyOfSchema, metadata)
    case None => doExtractObject(cursor, cs, metadata)
  }

  private def doExtractObject(cursor: JsonCursor, cs: ClassSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    def matchOnlyWhens(property: Property, onlyWhenAnnotations: List[SerializableOnlyWhen], extractedPropertyValue: Either[List[ValidationError], Any]) = {
      val found = onlyWhenAnnotations.find { case SerializableOnlyWhen(path, expectedValue) =>
        val valueFromPath: JValue = cursor.navigate(path).json
        JsonCompare.equals(valueFromPath, expectedValue)
      }
      if (found.isDefined) {
        None
      } else {
        DefaultValue.getDefaultValue[Any](property.metadata) match {
          case Some(v) if Right(v) == extractedPropertyValue => None // Allow default value even when field is otherwise rejected
          case _ => Some(OnlyWhenMismatch(onlyWhenAnnotations))
        }
      }
    }

    def matchOnlyWhenAlls(property: Property, onlyWhenAnnotations: List[SerializableOnlyWhen], extractedPropertyValue: Either[List[ValidationError], Any]) = {
      val found = onlyWhenAnnotations.forall { case SerializableOnlyWhen(path, expectedValue) =>
        val valueFromPath: JValue = cursor.navigate(path).json
        JsonCompare.equals(valueFromPath, expectedValue)
      }
      if (found) {
        None
      } else {
        DefaultValue.getDefaultValue[Any](property.metadata) match {
          case Some(v) if Right(v) == extractedPropertyValue => None // Allow default value even when field is otherwise rejected
          case _ => Some(OnlyWhenAllMismatch(onlyWhenAnnotations))
        }
      }
    }

    def validationError(propertyValueCursor: JsonCursor, mismatches: (Option[ValidationRuleViolation], Option[ValidationRuleViolation])): Either[List[ValidationError], Nothing] =
      Left((mismatches._1.toList ++ mismatches._2.toList).map(violation => ValidationError(propertyValueCursor.path, propertyValueCursor.json, violation)))

    cursor.json match {
      case o@JObject(values) =>
        val propertyResults: List[Either[List[ValidationError], Any]] = cs.properties
          .filterNot(_.synthetic)
          .map { property =>
            val propertyValueCursor = cursor.subCursor(o \ property.key, property.key)
            val valuePresent = propertyValueCursor.json match {
              case JNothing | JNull => false
              case _ => true
            }

            val onlyWhenAnnotations: List[SerializableOnlyWhen] = property.metadata.collect { case o:OnlyWhen if valuePresent => o.serializableForm }
            val onlyWhenAllAnnotations: List[SerializableOnlyWhen] = property.metadata.collect { case o:OnlyWhenAll if valuePresent => o.serializableForm }

            val extractedPropertyValue = SchemaValidatingExtractor.extract(propertyValueCursor, property.schema, property.metadata)

            val onlyWhenMismatch = (onlyWhenAnnotations, onlyWhenAllAnnotations) match {
              case (Nil, Nil) => (None, None)
              case (_, Nil) => (matchOnlyWhens(property, onlyWhenAnnotations, extractedPropertyValue), None)
              case (Nil, _) => (None, matchOnlyWhenAlls(property, onlyWhenAllAnnotations, extractedPropertyValue))
              case _ => (matchOnlyWhens(property, onlyWhenAnnotations, extractedPropertyValue), matchOnlyWhenAlls(property, onlyWhenAllAnnotations, extractedPropertyValue))
            }

            onlyWhenMismatch match {
              case (None, None) => extractedPropertyValue
              case mismatches => validationError(propertyValueCursor, mismatches)
            }
          }
        val unexpectedProperties = if (context.ignoreUnexpectedProperties) Nil else values
          .filterNot(pair => cs.properties.find(_.key == pair._1).isDefined)
          .filterNot(pair => pair._2 == JNull)
          .map(pair => ValidationError(cursor.subPath(pair._1), pair._2, UnexpectedProperty()))
        val errors: List[ValidationError] = propertyResults.collect { case Left(errors) => errors }.flatten ++ unexpectedProperties
        errors match {
          case Nil =>
            Right(instantiateCaseClass(cursor.path, cs.fullClassName, propertyResults.map(_.right.get)))
          case _ =>
            Left(errors)
        }
      case json => Left(List(ValidationError(cursor.path, json, UnexpectedType("object"))))
    }
  }

  private val constructorCache: collection.mutable.Map[String, Constructor[_]] = collection.mutable.Map.empty

  private def instantiateCaseClass(path: String, className: String, params: List[Any]) = {
    val constructor = this.synchronized {
      constructorCache.getOrElseUpdate(className, {
        val klass = Class.forName(className)
        val constructors = klass.getConstructors
        if (constructors.isEmpty) {
          throw new RuntimeException(s"Cannot find constructor for $klass")
        }
        constructors.apply(0)
      })
    }
    val constructorParams: List[Object] = params.asInstanceOf[List[Object]]
    try {
      constructor.newInstance(constructorParams: _*).asInstanceOf[AnyRef]
    } catch {
      case e: Exception => throw new DeserializationException(path, s"instantiating ${className} with ${constructorParams}", e)
    }
  }
}
