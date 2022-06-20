package fi.oph.scalaschema

import org.json4s.{JArray, JValue}
import org.json4s.JsonAST.{JNull, JString}

object ValueConversion {
  // Use sparingly! Only supports numbers, strings, booleans and Option
  def anyToJValue(x: Any): JValue = x match {
    case None => JNull
    case Some(x) => anyToJValue(x)
    case s: String => JString(s)
    case n: Number => Serializer.serializeNumber(n)
    case b: Boolean => Serializer.serializeBoolean(b)
    case a: List[Any] => JArray(a.map(anyToJValue(_)))
    case _ => throw new IllegalArgumentException("Type not supported here: " + x.getClass)
  }
}
