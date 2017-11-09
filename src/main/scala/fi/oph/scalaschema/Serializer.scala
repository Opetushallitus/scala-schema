package fi.oph.scalaschema

import java.time.format.DateTimeFormatter.ISO_INSTANT
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.util.Date

import fi.oph.scalaschema.SchemaPropertyProcessor.SchemaPropertyProcessor
import fi.oph.scalaschema.extraction.SchemaNotFoundException
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST._
import org.json4s.{DefaultFormats, Extraction, Formats, JValue}

import scala.reflect.runtime.{universe => ru}

object Serializer {
  implicit val format: Formats = new DefaultFormats() {}

  def serialize[T : ru.TypeTag](obj: T, context: SerializationContext): JValue = {
    serialize(obj, context.schemaFactory.createSchema[T], context)
  }

  def serialize(obj: Any, schema: Schema, context: SerializationContext): JValue = {
    implicit val rootSchema = schema
    implicit val ctx = context
    serializeWithSchema(obj, rootSchema)
  }

  private def serializeWithSchema(x: Any, schema: Schema)(implicit context: SerializationContext, rootSchema: Schema): JValue = {
    schema match {
      case s: ClassSchema => serializeObject(s, x)
      case s: ClassRefSchema =>
        val actualSchema = rootSchema.getSchema(s.fullClassName).getOrElse(throw new SchemaNotFoundException("", s.fullClassName))
        serializeWithSchema(x, actualSchema)
      case s: AnyOfSchema =>
        s.findAlternative(x) match {
          case Some(foundSchema) => serializeWithSchema(x, foundSchema)
          case _ => throw new RuntimeException("Schema not found for " + x + " as an implementation of " + s.fullClassName)
        }
      case s: FlattenedSchema => serializeWithSchema(s.getValue(x.asInstanceOf[AnyRef]), s.property.schema)
      case s: ReadFlattenedSchema => serializeWithSchema(x, s.classSchema)
      case s: OptionalSchema => serializeOption(s, x)
      case s: ListSchema => serializeList(s, x)
      case s: MapSchema => serializeMap(s, x)
      case s: StringSchema => serializeString(x)
      case s: NumberSchema => serializeNumber(x)
      case s: DateSchema => serializeDate(s, x)
      case s: BooleanSchema => serializeBoolean(x)
      case s: AnySchema => serializeAny(x)
      case s: AnyObjectSchema => serializeAny(x)
      case s: AnyListSchema => serializeAny(x)
    }
  }

  private def serializeOption(s: OptionalSchema, x: Any)(implicit context: SerializationContext, rootSchema: Schema): JValue = x match {
    case Some(x) => serializeWithSchema(x, s.itemSchema)
    case None => JNothing
    case x => serializeWithSchema(x, s.itemSchema)
  }

  private def serializeList(s: ListSchema, x: Any)(implicit context: SerializationContext, rootSchema: Schema): JValue = x match {
    case xs: List[_] => JArray(xs.map { x => serializeWithSchema(x, s.itemSchema)})
    case _ => throw new RuntimeException("Not a List: " + x)
  }

  private def serializeMap(s: MapSchema, x: Any)(implicit context: SerializationContext, rootSchema: Schema): JValue = x match {
    case xs: Map[String, _] => JObject(xs.toList.map { case (key, value) => JField(key, serializeWithSchema(value, s.itemSchema))})
    case _ => throw new RuntimeException("Not a List: " + x)
  }

  private def serializeObject(s: ClassSchema, x: Any)(implicit context: SerializationContext, rootSchema: Schema): JValue = JObject(s.properties.flatMap { p =>
    context.propertyProcessor(s, p).flatMap { p =>
      val value = s.getPropertyValue(p, x.asInstanceOf[AnyRef])
      serializeWithSchema(value, p.schema) match {
        case JNothing => if (context.omitEmptyFields) {
          None
        } else {
          Some(JField(p.key, JNull))
        }
        case jValue => Some(JField(p.key, jValue))
      }
    }
  })

  def serializeString(x: Any): JValue = x match {
    case x: String => JString(x)
    case _ => throw new RuntimeException("Not a String: " + x)
  }

  def serializeNumber(x: Any): JValue = x match {
    case x: Number => Extraction.decompose(x)
    case _ => throw new RuntimeException("Not a Number: " + x)
  }

  private def serializeDate(s: DateSchema, x: Any): JValue = x match {
    case x: LocalDate => JString(x.toString)
    case x: DateTime => JString(ISODateTimeFormat.dateTimeNoMillis.print(x))
    case x: LocalDateTime => JString(x.toString)
    case x: Date => JString(ISO_INSTANT.format(ZonedDateTime.ofInstant(x.toInstant, ZoneId.of("UTC"))))
    case x: ZonedDateTime => JString(x.toString)
    case _ => throw new RuntimeException("Not a date: " + x)
  }

  def serializeBoolean(x: Any): JValue = x match {
    case x: Boolean => JBool(x)
    case _ => throw new RuntimeException("Not a Boolean: " + x)
  }

  private def serializeAny(x: Any): JValue = x match {
    case x: JValue => x
    case _ => throw new RuntimeException("Not a JValue: " + x)
  }
}

case class SerializationContext(schemaFactory: SchemaFactory, propertyProcessor: SchemaPropertyProcessor = (s, p) => List(p), omitEmptyFields: Boolean = true)

object SchemaPropertyProcessor {
  type SchemaPropertyProcessor = (ClassSchema, Property) => List[Property]
}