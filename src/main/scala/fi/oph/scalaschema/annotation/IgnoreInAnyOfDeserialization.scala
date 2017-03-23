package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.RepresentationalMetadata

/**
  *  Tags a case class as ignored when deciding which alternative of AnyOfSchema (in other words, which implementation of a trait) is to be used.
  */
case class IgnoreInAnyOfDeserialization() extends RepresentationalMetadata
