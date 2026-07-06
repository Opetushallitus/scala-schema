package fi.oph.scalaschema.annotation

import scala.annotation.StaticAnnotation

/**
 * Used to tag computed JSON properties.
 *
 * Computed properties are omitted from generated schemas by default. A root
 * schema class can include selected computed properties with
 * @IncludeComputedProperty. See IncludeComputedProperty for owner class
 * limitations.
 */
class ComputedProperty extends StaticAnnotation
