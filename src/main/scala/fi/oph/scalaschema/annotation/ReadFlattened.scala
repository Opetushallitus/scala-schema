package fi.oph.scalaschema.annotation

import scala.annotation.StaticAnnotation

/**
  *  Allows a case class to be extracted from a flattened version of its JSON, meaning that for instance,
  *  if a case class has a single string field "value", it can be extracted from a string. This differes from
  *  the @Flatten annotation in that a @ReadFlattened class will always be serialized in its full form while a
  *  case class with @Flatten will be serialized in flattened form. Also, @ReadFlattened case class can have optional
  *  fields in addition to the single required field.
  */
case class ReadFlattened() extends StaticAnnotation
