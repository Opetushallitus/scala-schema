package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation._
import fi.oph.scalaschema.{Schema, _}
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.jackson.JsonMethods

object AnyOfExtractor {
  private def criteriaForSchema(schema: SchemaWithClassName, cursor: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema) = context.criteriaCache.synchronized {
    context.criteriaCache.getOrElseUpdate(schema.fullClassName, {
      discriminatorCriteria(cursor.path, schema, KeyPath.root)
    })
  }

  def extractAnyOf(cursor: JsonCursor, as: AnyOfSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = {
    val mapping: List[(SchemaWithClassName, CriteriaCollection)] = as.alternatives.filterNot(ignoredAlternative).map { schema =>
      (schema, criteriaForSchema(schema, cursor))
    }.sortBy(-_._2.weight)

    val matchingSchemas = mapping.collect {
      case (schema, criteria) if criteria.matches(cursor) =>
        (schema, criteria)
    }

    matchingSchemas match {
      case Nil =>
        val allowedAlternatives: List[(String, List[String])] = mapping.map { case (schema, criteria) => (schema.simpleName, criteria.apply(cursor)) }
        Left(List(ValidationError(cursor.path, cursor.json, NotAnyOf(allowedAlternatives.toMap))))
      case _ =>
        val maxWeight = matchingSchemas.head._2.weight
        val schemasWithMaximumNumberOfMatchingCriteria = matchingSchemas.filter { case (_, criteria) => criteria.weight == maxWeight }
        schemasWithMaximumNumberOfMatchingCriteria match {
          case List((schema, criteria)) =>
            SchemaValidatingExtractor.extract(cursor, schema, metadata)
          case _ =>
            throw new TooManyMatchingCasesException(cursor.path, schemasWithMaximumNumberOfMatchingCriteria, cursor.json)
        }
    }
  }

  private def ignoredAlternative(schema: SchemaWithClassName) = {
    schema.metadata.contains(IgnoreInAnyOfDeserialization())
  }

  private def discriminatorCriteria(contextPath: String, schema: Schema, keyPath: KeyPath)(implicit context: ExtractionContext, rootSchema: Schema): CriteriaCollection = schema match {
    case s: ClassRefSchema =>
      discriminatorCriteria(contextPath, SchemaResolver.resolveSchema(s, contextPath), keyPath)
    case s: ClassSchema =>
      val discriminatorProps: List[Property] = s.properties.filter(_.metadata.contains(Discriminator()))

      val onlyWhens: List[DiscriminatorCriterion] = s.metadata.collect({ case o:OnlyWhen => o}) match {
        case Nil => Nil
        case onlyWhens =>
          List(OnlyWhenCriteria(keyPath, onlyWhens.map(_.serializableForm)))
      }

      val criteria = discriminatorProps match {
        case Nil =>
          NoOtherPropertiesThan(keyPath, s.properties.map(_.key)) :: (s.properties.flatMap(propertyMatchers(contextPath, keyPath, _)))
        case props =>
          props.flatMap(propertyMatchers(contextPath, keyPath, _))
      }

      CriteriaCollection(criteria ++ onlyWhens)
    case s: FlattenedSchema => CriteriaCollection(List(Flattened(keyPath, s, discriminatorCriteria(contextPath, s.itemSchema, keyPath))))
    case s: NumberSchema => CriteriaCollection(List(IsNumeric(keyPath, s)))
    case s: StringSchema => CriteriaCollection(List(IsString(keyPath, s)))
    case s: BooleanSchema => CriteriaCollection(List(IsBoolean(keyPath, s)))
    case s: DateSchema => CriteriaCollection(List(IsDate(keyPath, s)))
    case s: ListSchema => CriteriaCollection(List(IsArray(keyPath, s)))
    case _ => throw new RuntimeException(s"Only ClassSchema, ClassRefSchema supported as alternatives in AnyOfSchema (found ${schema})")
  }

  private def propertyMatchers(contextPath: String, keyPath:KeyPath, property: Property)(implicit context: ExtractionContext, rootSchema: Schema): List[DiscriminatorCriterion] = {
    val propertyPath = keyPath.concat(property.key)
    property.schema match {
      case s: OptionalSchema if !property.metadata.contains(Discriminator()) => Nil // Optional attribute are required only when marked with @Discriminator
      case OptionalSchema(s) => propertyMatchers(contextPath, keyPath, property.copy(schema = s)) // OptionalSchema with Discriminator => dig deeper
      case s: StringSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: NumberSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: BooleanSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: ClassRefSchema => propertyMatchers(contextPath, keyPath, property.copy(schema = SchemaResolver.resolveSchema(s, contextPath)))
      case s: ClassSchema =>
        List(PropertyExists(propertyPath)) ++ s.properties.flatMap { nestedProperty =>
          discriminatorCriteria(JsonCursor.subPath(contextPath, nestedProperty.key), s, propertyPath).criteria
        }
      case s =>
        List(PropertyExists(propertyPath))
    }
  }.distinct


  case class KeyPath(path: List[String]) {
    def apply(value: JsonCursor): JsonCursor = path.foldLeft(value) { case (v, pathElem) => v.navigate(pathElem) }

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
    def apply(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String]
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
    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = criteria.flatMap(c => c.apply(json))
    def matches(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema) = apply(json).isEmpty
    def description = criteria.map(_.description).mkString(" and ")
  }

  case class Flattened(val keyPath: KeyPath, schema: FlattenedSchema, wrappedSchemaCriteria: CriteriaCollection) extends DiscriminatorCriterion {
    override def description: String = wrappedSchemaCriteria.description

    override def weight: Int = wrappedSchemaCriteria.weight

    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = wrappedSchemaCriteria.apply(json)
  }

  case class PropertyExists(val keyPath: KeyPath) extends DiscriminatorCriterion {
    def apply(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = keyPath(value).json match {
      case JNothing => List(toString)
      case _ => Nil
    }
    def description = "exists"
    def weight = 100
  }

  abstract class TypeMatcher(val description: String) extends DiscriminatorCriterion {
    override def apply(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = matchType(value) match {
      case true => Nil
      case _ => List(description)
    }
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean
    def weight = 100
  }

  case class IsNumeric(val keyPath: KeyPath, schema: NumberSchema) extends TypeMatcher("numeric") {
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean = NumberExtractor.extractExisting(value, schema, Nil).isRight
  }

  case class IsString(val keyPath: KeyPath, schema: StringSchema) extends TypeMatcher("string") {
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean = StringExtractor.extractExisting(value, schema, Nil)isRight

    override def weight = super.weight - 1 // prefer the more specific matchers
  }

  case class IsBoolean(val keyPath: KeyPath, schema: BooleanSchema) extends TypeMatcher("boolean") {
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean = BooleanExtractor.extractExisting(value, schema, Nil).isRight
  }

  case class IsDate(val keyPath: KeyPath, schema: DateSchema) extends TypeMatcher("date") {
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean = DateExtractor.extractDate(value, schema, Nil).isRight
  }

  case class IsArray(val keyPath: KeyPath, schema: ListSchema) extends TypeMatcher("array") {
    def matchType(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): Boolean = value.json match {
      case arr: JArray => true
      case _ => false
    }
  }

  case class PropertyEnumValues(val keyPath: KeyPath, schema: Schema, enumValues: List[Any]) extends DiscriminatorCriterion {
    def apply(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = PropertyExists(keyPath)(value) match {
      case Nil =>
        val actualValue = SchemaValidatingExtractor.extract(keyPath(value), schema, Nil) // <- Equality checking occurs here: the extractor verifies that enum values do match
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

  case class OnlyWhenCriteria(val keyPath: KeyPath, criteria: List[SerializableOnlyWhen]) extends DiscriminatorCriterion {
    def weight = 10000

    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = {
      val valueAtKeyPath = keyPath(json)
      val found = criteria.find { case SerializableOnlyWhen(path, value) =>
        val valueAtExpectedPosition = valueAtKeyPath.navigate(path).json
        JsonCompare.equals(valueAtExpectedPosition, value)
      }
      found match {
        case Some(matching) => Nil
        case None => List(description)
      }
    }

    override def description: String = {
      criteria.map { case SerializableOnlyWhen(path, value) => s"${path}=${JsonMethods.compact(value)}" }.mkString(" or ")
    }
  }

  case class NoOtherPropertiesThan(keyPath: KeyPath, keys: List[String]) extends DiscriminatorCriterion {
    def apply(value: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = PropertyExists(keyPath)(value) match {
      case Nil =>
        keyPath(value).json match {
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