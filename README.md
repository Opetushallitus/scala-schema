## scala-schema

Generate a [JSON schema](http://json-schema.org/) from Scala classes. 

- Create schema from any case class
- Supports case classes, lists, strings, dates, numbers, booleans
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

You can use annotations to add a description and set some constraints, like this

```scala
import fi.oph.scalaschema.annotation.{MaxValue, MinValue, Description}

@Description("A cat")
case class AnnotatedCat(
  @MinValue(3) @MaxValue(4)
  feet: Int,
  @RegularExpression(".*")
  name: String
)
```

You can add support for your custom annotations too, if you wish. Like

```scala
object ExampleWithCustomAnnotations extends App {
  val annotations = classOf[ReadOnly] :: SchemaFactory.defaultAnnotations
  val schema: Schema = SchemaFactory(annotations).createSchema(classOf[AnnotatedCat])
  val schemaAsJson: JValue = schema.toJson
  val schemaAsString = JsonMethods.pretty(schemaAsJson)
  println(schemaAsString)
}

case class ReadOnly(why: String) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject) = appendToDescription(obj, why)
}

case class ReadOnlyCat(
  @ReadOnly("Please don't mutate")
  feet: Int = 4
)
```

More examples and a pretty much full feature list can be found in this [test file](src/test/scala/fi/oph/scalaschema/JsonSchemaTest.scala).

### Maven

Add Jitpack.io as a repository:

```xml
<repositories>
  ...
  <repository>
    <id>jitpack.io</id>
    <url>https://jitpack.io</url>
  </repository>
</repositories>
```

Then add scala-schema as dependency

```xml
<dependencies>
  ...
  <dependency>
    <groupId>com.github.Opetushallitus</groupId>
    <artifactId>scala-schema</artifactId>
    <version>1.0</version>
  </dependency>
</dependencies>
```
