package fi.oph.scalaschema.annotation

import fi.oph.scalaschema._

import scala.reflect.ClassTag

case class EnumValue(value: Any) extends RepresentationalMetadata

object EnumValue {
  def addEnumValues(schema: Schema, newEnumValues: List[Any]): Schema = (schema, newEnumValues) match {
    case (_, Nil) => schema
    case (x: StringSchema, _) => x.copy(enumValues = concatenateEnumValues[String](x.enumValues, newEnumValues))
    case (x: BooleanSchema, _) => x.copy(enumValues = concatenateEnumValues[Boolean](x.enumValues, newEnumValues))
    case (x: NumberSchema, _) => x.copy(enumValues = concatenateEnumValues[Number](x.enumValues, newEnumValues))
    case (x: OptionalSchema, _) => x.mapItems(elementSchema => addEnumValues(elementSchema, newEnumValues).asInstanceOf[ElementSchema])
    case (x: ListSchema, _) => x.mapItems(elementSchema => addEnumValues(elementSchema, newEnumValues).asInstanceOf[ElementSchema])
    case _ => throw new UnsupportedOperationException("EnumValue not supported for " + schema)
  }

  private def concatenateEnumValues[T](enumValues: Option[List[T]], newEnumValues: List[Any])(implicit tag: ClassTag[T]): Option[List[T]] = {
    newEnumValues.foreach(tag.runtimeClass.cast) // Make sure each object is an instance of T
    (enumValues.toList.flatten ++ newEnumValues).distinct match {
      case Nil => None
      case values => Some(values.asInstanceOf[List[T]])
    }
  }
}
