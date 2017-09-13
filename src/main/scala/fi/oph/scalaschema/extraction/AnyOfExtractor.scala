package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{Discriminator, IgnoreInAnyOfDeserialization}
import fi.oph.scalaschema.{DateSchema, Schema, _}
import org.json4s.JsonAST.JObject
import org.json4s._

object AnyOfExtractor {
  private def criteriaForSchema(schema: SchemaWithClassName)(implicit context: ExtractionContext, rootSchema: Schema) = context.criteriaCache.synchronized {
    context.criteriaCache.getOrElseUpdate(schema.fullClassName, {
      discriminatorCriteria(schema, KeyPath.root)
    })
  }

  def extractAnyOf(json: JValue, as: AnyOfSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = {
    val mapping: List[(SchemaWithClassName, CriteriaCollection)] = as.alternatives.filterNot(ignoredAlternative).map { schema =>
      (schema, criteriaForSchema(schema))
    }.sortBy(-_._2.weight)

    val matchingSchemas = mapping.collect {
      case (schema, criteria) if criteria.matches(json) =>
        (schema, criteria)
    }

    matchingSchemas match {
      case Nil =>
        val allowedAlternatives: List[(String, List[String])] = mapping.map { case (schema, criteria) => (schema.simpleName, criteria.apply(json)) }
        Left(List(ValidationError(context.path, json, NotAnyOf(allowedAlternatives))))
      case _ =>
        val maxWeight = matchingSchemas.head._2.weight
        val schemasWithMaximumNumberOfMatchingCriteria = matchingSchemas.filter { case (_, criteria) => criteria.weight == maxWeight }
        schemasWithMaximumNumberOfMatchingCriteria match {
          case List((schema, criteria)) =>
            SchemaValidatingExtractor.extract(json, schema, metadata)
          case _ =>
            throw new TooManyMatchingCasesException(context.path, schemasWithMaximumNumberOfMatchingCriteria, json)
        }
    }
  }

  private def ignoredAlternative(schema: SchemaWithClassName) = {
    schema.metadata.contains(IgnoreInAnyOfDeserialization())
  }

  private def discriminatorCriteria(schema: SchemaWithClassName, keyPath: KeyPath)(implicit context: ExtractionContext, rootSchema: Schema): CriteriaCollection = schema match {
    case s: ClassRefSchema =>
      discriminatorCriteria(SchemaResolver.resolveSchema(s), keyPath)
    case s: ClassSchema =>
      val discriminatorProps: List[Property] = s.properties.filter(_.metadata.contains(Discriminator()))
      discriminatorProps match {
        case Nil =>
          CriteriaCollection(NoOtherPropertiesThan(keyPath, s.properties.map(_.key)) :: (s.properties.flatMap(propertyMatchers(keyPath, _))))
        case props =>
          CriteriaCollection(props.flatMap(propertyMatchers(keyPath, _)))
      }
    case _ => throw new RuntimeException(s"Only ClassSchema, ClassRefSchema supported as alternatives in AnyOfSchema (found ${schema})")
  }

  private def propertyMatchers(keyPath:KeyPath, property: Property)(implicit context: ExtractionContext, rootSchema: Schema): List[DiscriminatorCriterion] = {
    val propertyPath = keyPath.concat(property.key)
    property.schema match {
      case s: OptionalSchema if !property.metadata.contains(Discriminator()) => Nil // Optional attribute are required only when marked with @Discriminator
      case OptionalSchema(s) => propertyMatchers(keyPath, property.copy(schema = s)) // OptionalSchema with Discriminator => dig deeper
      case s: StringSchema if s.enumValues.isDefined =>  List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: NumberSchema if s.enumValues.isDefined =>  List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: BooleanSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: ClassRefSchema => propertyMatchers(keyPath, property.copy(schema = SchemaResolver.resolveSchema(s)))
      case s: ClassSchema =>
        List(PropertyExists(propertyPath)) ++ s.properties.flatMap { nestedProperty =>
          discriminatorCriteria(s, propertyPath).criteria
        }
      case s =>
        List(PropertyExists(propertyPath))
    }
  }.distinct


  case class KeyPath(path: List[String]) {
    def apply(value: JValue) = path.foldLeft(value) { case (v, pathElem) => v \ pathElem }
    def plusSpace = path match {
      case Nil => ""
      case more => toString + " "
    }
    override def toString = path.mkString(".")
    def concat(pathElem: String) = KeyPath(path ++ List(pathElem))
  }
  object KeyPath {
    val root = KeyPath(List())
  }

  trait DiscriminatorCriterion {
    def keyPath: KeyPath
    def apply(value: JValue)(implicit context: ExtractionContext, rootSchema: Schema): List[String]
    def description: String
    def withKeyPath(s: String) = keyPath match {
      case KeyPath(Nil) => s
      case _ => s"""property "${keyPath}" ${s}"""
    }
    def weight: Int
    override def toString = withKeyPath(description)
  }

  case class CriteriaCollection(criteria: List[DiscriminatorCriterion]) {
    lazy val weight = criteria.map(_.weight).sum
    def apply(json: JValue)(implicit context: ExtractionContext, rootSchema: Schema) = criteria.flatMap(c => c.apply(json))
    def matches(json: JValue)(implicit context: ExtractionContext, rootSchema: Schema) = apply(json).isEmpty
  }

  case class PropertyExists(val keyPath: KeyPath) extends DiscriminatorCriterion {
    def apply(value: JValue)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = keyPath(value) match {
      case JNothing => List(toString)
      case _ => Nil
    }
    def description = "exists"
    def weight = 100
  }

  case class PropertyEnumValues(val keyPath: KeyPath, schema: Schema, enumValues: List[Any]) extends DiscriminatorCriterion {
    def apply(value: JValue)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = PropertyExists(keyPath)(value) match {
      case Nil =>
        val actualValue = SchemaValidatingExtractor.extract(keyPath(value), schema, Nil)
        actualValue match {
          case Right(actualValue) =>
            Nil
          case Left(errors) =>
            List(toString)
        }
      case errors => errors
    }

    def description = enumValues match {
      case List(singleValue) => s"= $singleValue"
      case _ => s"in [${enumValues.mkString(", ")}]"
    }

    def weight = 10000
  }

  case class NoOtherPropertiesThan(keyPath: KeyPath, keys: List[String]) extends DiscriminatorCriterion {
    def apply(value: JValue)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = PropertyExists(keyPath)(value) match {
      case Nil =>
        keyPath(value) match {
          case JObject(values) =>
            values.toList.map(_._1).filterNot(keys.contains(_)) match {
              case Nil => Nil
              case unexpected => List(withKeyPath(s"allowed properties [${keys.mkString(", ")}] do not contain [${unexpected.mkString(", ")}]"))
            }
          case _ =>
            List(withKeyPath("object expected"))
        }
      case errors => errors
    }
    def description = s"no other properties than [${keys.mkString(", ")}]"
    def weight = 1
  }
}
