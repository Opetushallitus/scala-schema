package fi.oph.scalaschema.extraction

import org.json4s.{JNothing, JNull, JValue}

object JsonCompare {
  def equals(a: JValue, b: JValue) = nullToNothing(a) == nullToNothing(b)

  private def nullToNothing(x: JValue) = x match {
    case JNull => JNothing
    case x => x
  }
}
