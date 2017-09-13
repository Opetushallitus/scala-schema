package fi.oph.scalaschema
import fi.oph.scalaschema.extraction._
import org.json4s._
import org.json4s.jackson.JsonMethods

import scala.reflect.runtime.{universe => ru}

object SchemaValidatingExtractor {
  private val noErrors = Nil : List[ValidationError]

  def extract[T](json: JValue)(implicit context: ExtractionContext, tag: ru.TypeTag[T]): Either[List[ValidationError], T] = {
    val rootSchema = context.schemaFactory.createSchema[T]
    extract(json, rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[T])
  }

  def extract[T](json: String)(implicit context: ExtractionContext, tag: ru.TypeTag[T]): Either[List[ValidationError], T] = {
    extract(JsonMethods.parse(json))
  }

  def extract(json: JValue, klass: Class[_])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    val rootSchema = context.schemaFactory.createSchema(klass.getName)
    extract(json, rootSchema, Nil)(context, rootSchema).right.map(_.asInstanceOf[AnyRef])
  }

  def extract(json: JValue, schema: Schema, metadata: List[Metadata])(implicit context: ExtractionContext, rootSchema: Schema): Either[List[ValidationError], Any] = {
    schema match {
      case os: OptionalSchema => OptionalExtractor.extractOptional(json, os, metadata)
      case ss: StringSchema => StringExtractor.extract(json, ss, metadata)
      case ns: NumberSchema => NumberExtractor.extract(json, ns, metadata)
      case bs: BooleanSchema => BooleanExtractor.extract(json, bs, metadata)
      case as: AnySchema => Right(json)
      case _ => extractRequired(json, metadata) { schema match {
        case ls: ListSchema => ListExtractor.extractList(json, ls, metadata)
        case ms: MapSchema => MapExtractor.extractMap(json, ms, metadata)
        case ds: DateSchema => DateExtractor.extractDate(json, ds, metadata)
        case cs: SchemaWithClassName =>
          json match {
            case _: JObject =>
              (context.customSerializerFor(cs), cs) match {
                case (Some(serializer), cs: SchemaWithClassName) => serializer.extract(json, cs, metadata)
                case (_, cs: ClassRefSchema) => extract(json, SchemaResolver.resolveSchema(cs), metadata)
                case (_, cs: ClassSchema) => ObjectExtractor.extractObject(json, cs, metadata)
                case (_, as: AnyOfSchema) => AnyOfExtractor.extractAnyOf(json, as, metadata)
              }
            case _ =>
              Left(List(ValidationError(context.path, json, UnexpectedType("object"))))
          }
        case _ => throw new RuntimeException(s"Unexpected schema type ${schema}")
      }}
    }
  }

  private def extractRequired[T](json: JValue, metadata: List[Metadata])(doExtract: => Either[List[ValidationError], T])(implicit context: ExtractionContext) = json match {
    case JNothing =>
      Left(List(ValidationError(context.path, json, MissingProperty())))
    case _ =>
      doExtract
  }
}









