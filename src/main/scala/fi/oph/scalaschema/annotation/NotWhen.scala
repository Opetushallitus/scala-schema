package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.extraction.JsonCompare
import fi.oph.scalaschema.{Metadata, ValueConversion}
import org.json4s.{JArray, JNothing, JNull, JValue, JsonAST}

/**
 *  Matches, if the path's value is not in the disallowed values list.
 *  ===Example usage===
 *  {{{
 *  case class Car(
 *    hasTowbar: Boolean
 *    @NotWhen("hasTowbar", false)
 *    trailer: Option[Trailer]
 *  )
 *  case class Trailer(model: String)
 *  }}}
 * @param path [[java.lang.String]] Object path
 * @param disallowedValues Disallowed values. Can be a [[scala.AnyValCompanion]], [[scala.None]] or a [[scala.collection.immutable.List]] of [[scala.AnyValCompanion]].
 */
case class NotWhen(path: String, disallowedValues: Any) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JsonAST.JObject): JsonAST.JObject = appendToDescription(obj, s"(Not when $path = ${disallowedValues match {
    case listValues: List[Any] => listValues.mkString(",")
    case value: Any => value.toString()
    case None => None
  }})")
  def serializableForm = SerializableNotWhen(path, disallowedValues match {
    case a: Any => ValueConversion.anyToJValueWithLists(a)
  })
}

object NotWhenMatcher {
  def matchValues(valueAtExpectedPosition: JValue, values: JValue) = (valueAtExpectedPosition, values) match {
    case (_, JArray(list)) => !list.filter(anyValue => JsonCompare.equals(valueAtExpectedPosition, anyValue)).isEmpty
    case (JNothing, JNull) => true
    case (_, JNull) => JsonCompare.equals(valueAtExpectedPosition, JNull)
    case (_, jVal: JValue) => JsonCompare.equals(valueAtExpectedPosition, jVal)
  }
}

case class SerializableNotWhen(path: String, values: JValue)