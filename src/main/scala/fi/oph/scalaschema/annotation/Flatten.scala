package fi.oph.scalaschema.annotation

import scala.annotation.StaticAnnotation

/**
  *  Case class that has exactly one field can be flattened in the schema using this annotation.
  */
case class Flatten() extends StaticAnnotation
