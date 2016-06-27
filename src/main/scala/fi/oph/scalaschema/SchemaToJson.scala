package fi.oph.scalaschema

import org.json4s.ext.JodaTimeSerializers
import org.json4s.{DefaultFormats, Formats, Extraction}
import org.json4s.JsonAST._

object SchemaToJson {
  private implicit val jsonFormats: Formats = new DefaultFormats {
    override def dateFormatter = {
      val format = super.dateFormatter
      format.setTimeZone(DefaultFormats.UTC)
      format
    }
  } ++ JodaTimeSerializers.all

  def toJsonSchema(t: Schema): JObject = {
    appendMetadata(toJsonSchemaWithoutMetadata(t), t.metadata)
  }

  private def toJsonSchemaWithoutMetadata(t: Schema): JObject = t match {
    case DateSchema(enumValues) => JObject(List("type" -> JString("string"), "format" -> JString("date")) ++ toEnumValueProperty(enumValues))
    case StringSchema(enumValues) => withMinLength(simpleObjectToJson("string", enumValues), Some(1))
    case BooleanSchema(enumValues) => simpleObjectToJson("boolean", enumValues)
    case NumberSchema(enumValues) => simpleObjectToJson("number", enumValues)
    case ListSchema(x) => JObject(("type") -> JString("array"), (("items" -> toJsonSchema(x))))
    case OptionalSchema(x) => toJsonSchemaWithoutMetadata(x)
    case t: ClassRefSchema => JObject(
      ("$ref" -> JString("#/definitions/" + t.simpleName))
    )
    case t: ClassSchema => JObject(List(
      ("type" -> JString("object")),
      ("properties" -> toJsonProperties(t.properties)),
      ("id" -> JString("#" + t.simpleName)),
      ("additionalProperties" -> JBool(false)),
      ("title" -> JString(t.titleName))
    ) ++ toRequiredProperties(t.properties).toList
      ++ toDefinitionProperty(t.definitions).toList
    )
    case AnyOfSchema(alternatives, _, _, definitions) => JObject(
      List("anyOf" -> JArray(alternatives.map(toJsonSchemaWithoutMetadata(_)))) ++ toDefinitionProperty(definitions).toList
    )
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
    enumValues.map(enumValues => ("enum", Extraction.decompose(enumValues)))
  }

  private def toJsonProperties(properties: List[Property]): JValue = {
    JObject(properties.map { property =>
        (property.key, appendMetadata(appendMetadata(toJsonSchemaWithoutMetadata(property.schema), property.metadata), property.schema.metadata))
    })
  }
  private def toRequiredProperties(properties: List[Property]): Option[(String, JValue)] = {
    val requiredProperties = properties.toList.filter(!_.schema.isInstanceOf[OptionalSchema])
    requiredProperties match {
      case Nil => None
      case _ => Some("required", JArray(requiredProperties.map{property => JString(property.key)}))
    }
  }

  private def toDefinitionProperty(definitions: List[SchemaWithClassName]): Option[(String, JValue)] = definitions.flatMap {
    case x: ClassSchema => List(x)
    case _ => Nil
  } match {
    case Nil => None
    case _ =>
      Some("definitions", JObject(definitions.map(definition => (definition.simpleName, toJsonSchema(definition)))))
  }

  private def appendMetadata(obj: JObject, metadata: List[Metadata]): JObject = {
    metadata.foldLeft(obj) { case (obj: JObject, metadata: Metadata) =>
      metadata.appendMetadataToJsonSchema(obj)
    }
  }
}
