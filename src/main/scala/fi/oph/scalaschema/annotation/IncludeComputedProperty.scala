package fi.oph.scalaschema.annotation

import scala.annotation.StaticAnnotation

/**
 * Includes one computed property in schemas created from the annotated root
 * schema class. The property suffix path is dot-separated in root-to-leaf
 * order, and the last segment is the computed property name.
 *
 * A single-segment suffix path includes the property for the owner class
 * everywhere in the root schema. A longer suffix path includes it only when
 * the current schema path ends with that suffix.
 *
 * Path-specific variants use the suffix path as part of generated JSON Schema
 * definition names. Use ordinary identifier-like path segments: Unicode
 * letters, digits and underscores are supported. Avoid paths that differ only
 * by case or by unusual punctuation, because unsupported characters may be
 * normalized and produce colliding definition names. Such collisions fail
 * during JSON Schema generation.
 *
 * The owner class must not be a nested class or a local class. For nested or
 * local classes the owner comparison does not match, and the computed property
 * is not included in the generated schema.
 */
case class IncludeComputedProperty(owner: Class[_], propertySuffixPath: String) extends StaticAnnotation
