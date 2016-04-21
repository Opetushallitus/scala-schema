package fi.oph.scalaschema

import org.json4s.ext.JodaTimeSerializers
import org.json4s.{DefaultFormats, Formats, Extraction}
import org.json4s.JsonAST._

object JsonFormats {
  val genericFormats: Formats =  new DefaultFormats {
    override def dateFormatter = {
      val format = super.dateFormatter
      format.setTimeZone(DefaultFormats.UTC)
      format
    }
  } ++ JodaTimeSerializers.all
}
object SchemaToJson {
  def toJsonSchema(t: Schema)(implicit ms: List[JsonMetadataSupport[_]]): JValue = t match {
    case DateSchema(enumValues) => JObject(List("type" -> JString("string"), "format" -> JString("date")) ++ toEnumValueProperty(enumValues))
    case StringSchema(enumValues) => withMinLength(simpleObjectToJson("string", enumValues), Some(1))
    case BooleanSchema(enumValues) => simpleObjectToJson("boolean", enumValues)
    case NumberSchema(enumValues) => simpleObjectToJson("number", enumValues)
    case ListSchema(x) => JObject(("type") -> JString("array"), (("items" -> toJsonSchema(x))))
    case OptionalSchema(x) => toJsonSchema(x)
    case t: ClassRefSchema => appendMetadata(
      JObject(
        ("$ref" -> JString("#/definitions/" + t.simpleName))
      ),
      t.metadata
    )
    case t: ClassSchema => appendMetadata(
      JObject(List(
        ("type" -> JString("object")),
        ("properties" -> toJsonProperties(t.properties)),
        ("id" -> JString("#" + t.simpleName)),
        ("additionalProperties" -> JBool(false)),
        ("title" -> JString(t.titleName))
      ) ++ toRequiredProperties(t.properties).toList
        ++ toDefinitionProperty(t.definitions).toList
      ),
      t.metadata
    )
    case AnyOfSchema(alternatives, _) => JObject(("anyOf" -> JArray(alternatives.map(toJsonSchema(_)))))
  }

  private def simpleObjectToJson(tyep: String, enumValues: Option[List[Any]]): JObject = {
    addOptionalField(JObject(List("type" -> JString(tyep))), toEnumValueProperty(enumValues))
  }

  private def withMinLength(obj: JObject, minLength: Option[Int]) = {
    addOptionalField(obj, minLength.map { len => ("minLength" -> JInt(len)) })
  }

  private def addOptionalField(obj: JObject, field: Option[(String, JValue)]) = field match {
    case Some((name, value)) => obj.merge(JObject(List((name, value))))
    case _ => obj
  }

  private def toEnumValueProperty(enumValues: Option[List[Any]]): Option[(String, JValue)] = {
    implicit val formats = JsonFormats.genericFormats
    enumValues.map(enumValues => ("enum", Extraction.decompose(enumValues)))
  }

  private def toJsonProperties(properties: List[Property])(implicit ms: List[JsonMetadataSupport[_]]): JValue = {
    JObject(properties.map { property =>
        (property.key, appendMetadata(toJsonSchema(property.schema).asInstanceOf[JObject], property.metadata))
    })
  }
  private def toRequiredProperties(properties: List[Property]): Option[(String, JValue)] = {
    val requiredProperties = properties.toList.filter(!_.schema.isInstanceOf[OptionalSchema])
    requiredProperties match {
      case Nil => None
      case _ => Some("required", JArray(requiredProperties.map{property => JString(property.key)}))
    }
  }

  private def toDefinitionProperty(definitions: List[SchemaWithClassName])(implicit ms: List[JsonMetadataSupport[_]]): Option[(String, JValue)] = definitions.flatMap {
    case x: ClassSchema => List(x)
    case _ => Nil
  } match {
    case Nil => None
    case _ =>
      Some("definitions", JObject(definitions.map(definition => (definition.simpleName, toJsonSchema(definition)))))
  }

  private def appendMetadata(obj: JObject, metadata: List[Metadata])(implicit ms: List[JsonMetadataSupport[_]]): JObject = {
    metadata.foldLeft(obj) { case (obj: JObject, metadata) =>
      ms.foldLeft(obj) { case (obj, metadataSupport) =>
        metadataSupport.appendMetadataToJsonSchema(obj, metadata)
      }
    }
  }
}
