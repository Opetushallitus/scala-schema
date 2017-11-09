package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation._
import fi.oph.scalaschema.{Schema, _}
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.jackson.JsonMethods

object AnyOfExtractor {
  private def criteriaForSchema(schema: SchemaWithClassName, cursor: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema) = context.criteriaCache.synchronized {
    context.criteriaCache.getOrElseUpdate(schema.fullClassName + ":" + schema.getClass.getName, {
      discriminatorCriteria(cursor.path, schema, KeyPath.root)
    })
  }

  def extractAnyOf(cursor: JsonCursor, as: AnyOfSchema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = {
    val mapping: List[(SchemaWithClassName, DiscriminatorCriterion)] = as.alternatives.filterNot(ignoredAlternative).map { schema =>
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

  private def discriminatorCriteria(contextPath: String, schema: Schema, keyPath: KeyPath)(implicit context: ExtractionContext, rootSchema: Schema): DiscriminatorCriterion = schema match {
    case s: ClassRefSchema =>
      discriminatorCriteria(contextPath, s.resolve(context.schemaFactory), keyPath)
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

      AllOfCriterion(criteria ++ onlyWhens)
    case s: FlattenedSchema => AllOfCriterion(List(Flattened(keyPath, s, discriminatorCriteria(contextPath, s.property.schema, keyPath))))
    case s: ReadFlattenedSchema => OneOfCriteria(s.asAnyOfSchema.alternatives.map(alt => discriminatorCriteria(contextPath, alt, keyPath)))
    case s: NumberSchema => IsNumeric(keyPath, s)
    case s: StringSchema => IsString(keyPath, s)
    case s: BooleanSchema => IsBoolean(keyPath, s)
    case s: DateSchema => IsDate(keyPath, s)
    case s: ListSchema => IsArray(keyPath, s)
    case _ => throw new RuntimeException(s"Unsupported alternative schema in AnyOfSchema: ${schema}")
  }

  private def propertyMatchers(contextPath: String, keyPath:KeyPath, property: Property)(implicit context: ExtractionContext, rootSchema: Schema): List[DiscriminatorCriterion] = {
    val propertyPath = keyPath.concat(property.key)
    property.schema match {
      case s: OptionalSchema if !property.metadata.contains(Discriminator()) => Nil // Optional attribute are required only when marked with @Discriminator
      case OptionalSchema(s) => propertyMatchers(contextPath, keyPath, property.copy(schema = s)) // OptionalSchema with Discriminator => dig deeper
      case s: StringSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: NumberSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: BooleanSchema if s.enumValues.isDefined => List(PropertyEnumValues(propertyPath, s, s.enumValues.get))
      case s: ClassRefSchema => propertyMatchers(contextPath, keyPath, property.copy(schema = s.resolve(context.schemaFactory)))
      case s: ClassSchema =>
        List(PropertyExists(propertyPath)) ++ s.properties.flatMap { nestedProperty =>
          discriminatorCriteria(JsonCursor.subPath(contextPath, nestedProperty.key), s, propertyPath) match {
            case AllOfCriterion(criteria) => criteria
            case other => List(other)
          }
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
    def matches(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema) = apply(json).isEmpty
    def description: String
    def withKeyPath(s: String) = keyPath match {
      case KeyPath(Nil) => s
      case _ => s"""property "${keyPath}" ${s}"""
    }
    def weight: Int
    override def toString = withKeyPath(description)
  }

  case class AllOfCriterion(criteria: List[DiscriminatorCriterion]) extends DiscriminatorCriterion{
    lazy val weight = criteria.map(_.weight).sum
    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = criteria.flatMap(c => c.apply(json))
    def description = criteria.map(_.toString).mkString(" and ")
    override def keyPath: KeyPath = criteria(0).keyPath
  }

  case class OneOfCriteria(criteria: List[DiscriminatorCriterion]) extends DiscriminatorCriterion {
    lazy val weight = criteria.map(_.weight).sum / criteria.length
    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = if (criteria.find(_.apply(json).isEmpty).isDefined) Nil else List(description)
    def description = criteria.map(_.toString).mkString(" or ")
    override def keyPath: KeyPath = criteria(0).keyPath
  }

  case class Flattened(val keyPath: KeyPath, schema: FlattenedSchema, wrappedSchemaCriterion: DiscriminatorCriterion) extends DiscriminatorCriterion {
    override def description: String = wrappedSchemaCriterion.description

    override def weight: Int = wrappedSchemaCriterion.weight

    def apply(json: JsonCursor)(implicit context: ExtractionContext, rootSchema: Schema): List[String] = wrappedSchemaCriterion.apply(json)
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