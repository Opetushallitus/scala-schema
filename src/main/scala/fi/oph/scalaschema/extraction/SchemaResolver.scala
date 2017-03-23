package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.{ClassRefSchema, ExtractionContext}

object SchemaResolver {
  def resolveSchema(cs: ClassRefSchema)(implicit context: ExtractionContext) = {
    context.rootSchema.getSchema(cs.fullClassName).getOrElse(throw new SchemaNotFoundException(context.path, cs.fullClassName))
  }
}
