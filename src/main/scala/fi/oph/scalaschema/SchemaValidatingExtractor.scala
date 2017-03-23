package fi.oph.scalaschema

import fi.oph.scalaschema.extraction._
import org.json4s._

import scala.runtime.Nothing$

object SchemaValidatingExtractor {
  private val noErrors = Nil : List[ValidationError]

  def extract[T](json: JValue)(implicit context: ExtractionContext, mf: ClassManifest[T]): Either[List[ValidationError], T] = {
    val klass = mf.runtimeClass
    if (klass == classOf[Nothing$]) {
      throw new RuntimeException("Cannot extract as Nothing$. Did you forget to add type to your extract call? Try something like extract[MyClass)(json).")
    }
    extract(json, klass).right.map(_.asInstanceOf[T])
  }

  def extract[T](json: String)(implicit context: ExtractionContext, mf: ClassManifest[T]): Either[List[ValidationError], T] = {
    extract(Json.read[JValue](json))
  }

  def extract(json: JValue, klass: Class[_])(implicit context: ExtractionContext): Either[List[ValidationError], AnyRef] = {
    context.rootSchema.getSchema(klass.getName) match {
      case Some(schema) => extract(json, schema, Nil).right.map(_.asInstanceOf[AnyRef])
      case None => throw new SchemaNotFoundException(context.path, klass.getName)
    }
  }

  def extract(json: JValue, schema: Schema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Any] = {
    schema match {
      case os: OptionalSchema => OptionalExtractor.extractOptional(json, os, metadata)
      case _ => extractRequired(json) { schema match {
        case ls: ListSchema => ListExtractor.extractList(json, ls, metadata)
        case ss: StringSchema => StringExtractor.extractString(json, ss, metadata)
        case ns: NumberSchema => NumberExtractor.extractNumber(json, ns, metadata)
        case bs: BooleanSchema => BooleanExtractor.extractBoolean(json, bs, metadata)
        case ds: DateSchema => DateExtractor.extractDate(json, ds, metadata)
        case cs: SchemaWithClassName =>
          json match {
            case _: JObject =>
              (context.customSerializerFor(cs), schema) match {
                case (Some(serializer), cs: SchemaWithClassName) => serializer.extract(json, cs, metadata)
                case (_, cs: ClassRefSchema) => extract(json, SchemaResolver.resolveSchema(cs), metadata)
                case (_, cs: ClassSchema) => ObjectExtractor.extractObject(json, cs, metadata)
                case (_, as: AnyOfSchema) => AnyOfExtractor.extractAnyOf(json, as, metadata)
              }
            case _ =>
              Left(List(ValidationError(context.path, json, UnexpectedType("object"))))
          }
      }}
    }
  }

  private def extractRequired[T](json: JValue)(doExtract: => Either[List[ValidationError], T])(implicit context: ExtractionContext) = json match {
    case JNothing =>
      Left(List(ValidationError(context.path, json, MissingProperty())))
    case _ =>
      doExtract
  }
}









