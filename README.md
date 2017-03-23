## scala-schema

Generate a [JSON schema](http://json-schema.org/) from Scala classes 

- Create schema from any case class
- Export the schema as JSON
- Use the schema object directly for efficient JSON validation
- Extract JSON into case classes while validating on the way
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

You can tag any method in your case class with `@SyntheticProperty` so that it will also be considered as a property in your schema:

```scala
case class SyntheticCat() {
  @SyntheticProperty
  def name = "synthetic name"
}
```

More examples and a pretty much full feature list can be found in this [test file](src/test/scala/fi/oph/scalaschema/JsonSchemaTest.scala).

### Validation and extraction

```scala

package fi.oph.scalaschema

import fi.oph.scalaschema.SchemaValidatingExtractor.extract
import fi.oph.scalaschema.extraction.ValidationError
import org.json4s.jackson.JsonMethods

object ValidationExample extends App {
  implicit val context = ExtractionContext(SchemaFactory.default.createSchema(classOf[ValidationTestClass]))

  println("*** Successful object extraction ***")
  val validInput = JsonMethods.parse("""{"name": "john", "stuff": [1,2,3]}""")
  val extractionResult: Either[List[ValidationError], ValidationTestClass] = extract[ValidationTestClass](validInput)
  println(extractionResult)
  println("*** Validation failure ***")
  println(SchemaValidatingExtractor.extract[ValidationTestClass]("""{}"""))

}

case class ValidationTestClass(name: String, stuff: List[Int])

```

### How to use as dependency

The `scala-schema` library is currently maintained in two branches for scala versions 2.11 and 2.12.

It cannot be found in a Maven repository at the moment, but you can use [Jitpack.io](https://jitpack.io/) to 
depend on it anyway. Just follow the instructions below.

#### Maven

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
    <version>2.0_2.12</version>
  </dependency>
</dependencies>
```


#### SBT

Haven't used SBT for a while but it shouldn't be a biggie, as it should be able to use Jitpack.io like any other Maven repository, right?
