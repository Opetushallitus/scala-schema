package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.Serializer
import org.json4s.Extraction

protected [scalaschema] object AnyToJson {
  def anyToJValue(x: Any) = {
    import Serializer.format
    Extraction.decompose(x)
  }
}
