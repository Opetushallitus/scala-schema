package fi.oph.scalaschema.extraction

import org.json4s.jackson.Serialization
import org.json4s.{Extraction, _}
import scala.reflect.runtime.{universe => ru}

object Json {
  implicit val genericFormats: Formats =  new DefaultFormats {
    override def dateFormatter = {
      val format = super.dateFormatter
      format.setTimeZone(DefaultFormats.UTC)
      format
    }

    override val strictOptionParsing: Boolean = true
  }

  def write(x: AnyRef): String = {
    Serialization.write(x);
  }
  def writePretty(x: AnyRef): String = {
    Serialization.writePretty(x);
  }

  def read[A](json: String)(implicit tag: ru.TypeTag[A]) : A = {
    Serialization.read(json)
  }

  def toJValue(x: Any): JValue = x match {
    case x : AnyRef => Extraction.decompose(x)
    case _ => JDouble(x.asInstanceOf[Number].doubleValue())
  }

  def writeJsonish(t: Any) = t match {
    case t : AnyRef => Json.write(t)
    case _ => t.toString
  }
}
