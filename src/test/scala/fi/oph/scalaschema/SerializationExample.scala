package fi.oph.scalaschema

import fi.oph.scalaschema.SerializationExample.zoo
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

case class Zoo(animals: List[Animal])
case class Animal(name: String, age: Int)

object SerializationExample extends App {
  val context = SerializationContext(SchemaFactory.default)
  val zoo = Zoo(List(Animal("giraffe", 23)))
  val serialized: JValue = Serializer.serialize(zoo, context)
  val stringValue: String = JsonMethods.pretty(serialized)
  println(stringValue)

}

object CustomSerializationExample extends App {
  def hideAge(schema: ClassSchema, property: Property) = if (property.key == "age") Nil else List(property)
  val context = SerializationContext(SchemaFactory.default, hideAge)
  println(JsonMethods.pretty(Serializer.serialize(zoo, context)))
}