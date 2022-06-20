package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, ValueConversion}
import org.json4s.{JArray, JsonAST}

case class NotWhen(path: String, values: Option[List[Any]]) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Not when $path = ${values match {
    case Some(values) => values.mkString(",")
    case None => None
  }})")
  def serializableForm = SerializableNotWhen(path, values match {
    case Some(a) => Some(JArray(a.map(ValueConversion.anyToJValue(_))))
    case None => None
  })
}

case class SerializableNotWhen(path: String, values: Option[JArray])