package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.Serializer
import org.json4s.JValue
import org.json4s.JsonAST.{JNull, JString}

protected [scalaschema] object AnyToJson {
  // Use sparingly! Only supports numbers, strings, booleans and Option
  def anyToJValue(x: Any): JValue = x match {
    case None => JNull
    case Some(x) => anyToJValue(x)
    case s: String => JString(s)
    case n: Number => Serializer.serializeNumber(n)
    case b: Boolean => Serializer.serializeBoolean(b)
    case _ => throw new IllegalArgumentException("Type not supported here: " + x.getClass)
  }
}
