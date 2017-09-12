package fi.oph.scalaschema

object TestHelpers {
  def schemaOf(c: Class[_]) = SchemaFactory.default.createSchema(c)
}
