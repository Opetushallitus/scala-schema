package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.{Metadata, MetadataSupport}
import org.json4s.JsonAST.{JObject, JString}

object RegularExpression extends MetadataSupport[RegularExpression] {
  override def metadataClass = classOf[RegularExpression]

  override def appendMetadataToJsonSchema(obj: JObject, metadata: RegularExpression) = appendToDescription(obj.merge(JObject("pattern" -> JString(metadata.pattern))), "(Muoto: " + metadata.pattern + ")")
}

case class RegularExpression(pattern: String) extends Metadata