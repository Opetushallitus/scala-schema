package fi.oph.scalaschema.annotation

import fi.oph.scalaschema.RepresentationalMetadata

/**
  * Tags a field as a discriminator, so that it will be used to determine which alternative of AnyOfSchema (in other words, which implementation of a trait) should be used.
  */
case class Discriminator() extends RepresentationalMetadata
