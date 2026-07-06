package fi.oph.scalaschema.annotation

import scala.annotation.StaticAnnotation

/**
 * Includes one computed property in schemas created from the annotated root
 * schema class.
 *
 * The owner class must not be a nested class or a local class. For nested or
 * local classes the owner comparison does not match, and the computed property
 * is not included in the generated schema.
 */
case class IncludeComputedProperty(owner: Class[_], propertyName: String) extends StaticAnnotation
