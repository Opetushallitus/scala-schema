package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ClassRefSchema, ExtractionContext, Schema}

object SchemaResolver {
  def resolveSchema(cs: ClassRefSchema)(implicit context: ExtractionContext, rootSchema: Schema) = {
    rootSchema.getSchema(cs.fullClassName).getOrElse(throw new SchemaNotFoundException(context.path, cs.fullClassName))
  }
}
