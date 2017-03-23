package fi.oph.scalaschema

trait TestHelpers {
  def schemaOf(c: Class[_]) = SchemaFactory.default.createSchema(c)
}
