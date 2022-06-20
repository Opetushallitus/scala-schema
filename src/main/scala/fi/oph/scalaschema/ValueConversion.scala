package fi.oph.scalaschema

import org.json4s.{JArray, JValue}
import org.json4s.JsonAST.{JNull, JSet, JString}

object ValueConversion {
  def anyToJValue(x: Any): JValue = x match {
    // If support for a new type of value is needed, it can be added here
    case None => JNull
    case Some(x) => anyToJValue(x)
    case s: String => JString(s)
    case n: Number => Serializer.serializeNumber(n)
    case b: Boolean => Serializer.serializeBoolean(b)
    case _ => throw new IllegalArgumentException("Type not supported here: " + x.getClass)
  }
  def anyToJValueWithLists(x: Any): JValue = x match {
    // If support for a new type of value is needed, it can be added here
    case a: List[Any] => JArray(a.map(anyToJValueWithLists(_)))
    case b: Any => anyToJValue(b)
  }
}
