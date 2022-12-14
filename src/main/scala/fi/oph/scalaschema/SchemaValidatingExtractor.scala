package fi.oph.scalaschema
import fi.oph.scalaschema.extraction._
import org.json4s.{JString, _}
import org.json4s.jackson.JsonMethods

import scala.reflect.runtime.{universe => ru}

object SchemaValidatingExtractor {
  private val noErrors = Nil : List[ValidationError]

  def extract[T](json: JValue)(implicit context: ExtractionContext, tag: ru.TypeTag[T]): Either[List[ValidationError], T] = {
    val rootSchema = context.schemaFactory.createSchema[T]
    if(context.stripClassReferences) {
      extract(removeJsonField(json, "$class"), rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[T])
    } else {
      extract(json, rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[T])
    }
  }

  def extract[T](json: String)(implicit context: ExtractionContext, tag: ru.TypeTag[T]): Either[List[ValidationError], T] = {
    extract(JsonMethods.parse(json))
  }

  def removeJsonField(json: JValue, fieldName: String) = json removeField {
    case (key, _) if key == fieldName => true
    case _ => false
  }

  def extract(json: JValue, klass: Class[_])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    val rootSchema = context.schemaFactory.createSchema(klass.getName)
    if (context.stripClassReferences) {
      extract(removeJsonField(json, "$class"), rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[AnyRef])
    } else {
      extract(json, rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[AnyRef])
    }
  }

  def extract(json: JValue, schema: Schema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = {
    extract(JsonCursor(json), schema, metadata)
  }

  def extract(cursor: JsonCursor, schema: Schema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = {
    schema match {
      case os: OptionalSchema => OptionalExtractor.extractOptional(cursor, os, metadata)
      case ss: StringSchema => StringExtractor.extract(cursor, ss, metadata)
      case ns: NumberSchema => NumberExtractor.extract(cursor, ns, metadata)
      case bs: BooleanSchema => BooleanExtractor.extract(cursor, bs, metadata)
      case as: AnySchema => Right(cursor.json)
      case as: AnyObjectSchema => cursor.json match {
        case json: JObject => Right(json)
        case _ => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("object"))))
      }
      case as: AnyListSchema => cursor.json match {
        case json: JArray => Right(json)
        case _ => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("array"))))
      }
      case _ => extractRequired(cursor, metadata) { schema match {
        case ls: ListSchema => ListExtractor.extractList(cursor, ls, metadata)
        case ms: MapSchema => MapExtractor.extractMap(cursor, ms, metadata)
        case ds: DateSchema => DateExtractor.extractDate(cursor, ds, metadata)
        case fs: FlattenedSchema => ObjectExtractor.extractFlattenedObject(cursor, fs, metadata)
        case cs: SchemaWithClassName =>
          (context.customSerializerFor(cs), cs) match {
            case (Some(serializer), cs: SchemaWithClassName) => serializer.extract(cursor, cs, metadata)
            case (_, cs: ClassRefSchema) => extract(cursor, cs.resolve(context.schemaFactory), metadata)
            case (_, cs: ClassSchema) => ObjectExtractor.extractObject(cursor, cs, metadata)
            case (_, as: AnyOfSchema) => AnyOfExtractor.extractAnyOf(cursor, as, metadata)
            case _ => throw new RuntimeException(s"Unexpected schema type ${schema}")
          }
        case _ => throw new RuntimeException(s"Unexpected schema type ${schema}")
      }}
    }
  }

  private def extractRequired[T](cursor: JsonCursor, metadata: List[Metadata])(doExtract: => Either[List[ValidationError], T])(implicit context: ExtractionContext) = cursor.json match {
    case JNothing =>
      Left(List(ValidationError(cursor.path, cursor.json, MissingProperty())))
    case _ =>
      doExtract
  }
}









