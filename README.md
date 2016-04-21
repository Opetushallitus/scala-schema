## scala-schema

Generate a JSON schema from Scala classes. 

- Create schema from any case class
- Supports polymorphism via traits: finds trait implementations in same package
- Customize schema with annotations (like min/max size, description)

Uses json4s for JSON.

### Usage

Use `SchemaFactory` to create an object model representing your schema and convert it to JSON.

```scala
import fi.oph.scalaschema._

import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods

object ExampleApp extends App {
  val schema: Schema = SchemaFactory.default.createSchema(classOf[Cat])
  val schemaAsJson: JValue = schema.toJson
  val schemaAsString = JsonMethods.pretty(schemaAsJson)
  println(schemaAsString)
}

case class Cat(name: String)
```

Examples and feature list can be found in this [test file](src/test/scala/fi/oph/scalaschema/JsonSchemaTest.scala)
