## scala-schema

[![Build Status](https://travis-ci.org/Opetushallitus/scala-schema.svg?branch=scala-2.12)](https://travis-ci.org/Opetushallitus/scala-schema)

Generate a [JSON schema](http://json-schema.org/) from Scala classes 

- Create a Schema object from any `case class`
- Export the schema as JSON
- Use the schema object directly for efficient [JSON Validation and extraction into Scala objects](#validation-and-extraction), with machine and human-friendly validation error messages.
- [Serialize Scala objects](#serialization) into JSON. Do this way faster than with json4s serialization mechanism.
- Supports case classes, lists, strings, dates, numbers, booleans and maps (when keys are strings)
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
  val schema: Schema = SchemaFactory.default.createSchema[Cat]
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

You can add your custom annotations too, if you wish. Like

```scala
object ExampleWithCustomAnnotations extends App {
  val schema: Schema = SchemaFactory.default.createSchema[AnnotatedCat]
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

You can use `SchemaValidatingExtractor` to consume a JSON input and produce either

- a case class instance, or
- itemized validation errors

The beauty of this is that

- It's much more efficient than validating using a separate JSON schema validator
- It gives itemized, machine and human readable validation errors all of which point you to the exact location of the erroneous part in your JSON
- You don't need to write custom Serializer object for choosing between the correct implementation of a `trait`, instead you just tag the identifying fields with `@Discriminator` annotation.

```scala

package fi.oph.scalaschema

import fi.oph.scalaschema.SchemaValidatingExtractor.extract
import fi.oph.scalaschema.extraction.ValidationError
import org.json4s.jackson.JsonMethods

object ValidationExample extends App {
  implicit val context = ExtractionContext(SchemaFactory.default)

  println("*** Successful object extraction ***")
  val validInput = JsonMethods.parse("""{"name": "john", "stuff": [1,2,3]}""")
  val extractionResult: Either[List[ValidationError], ValidationTestClass] = extract[ValidationTestClass](validInput)
  println(extractionResult)
  println("*** Validation failure ***")
  println(SchemaValidatingExtractor.extract[ValidationTestClass]("""{}"""))
}

case class ValidationTestClass(name: String, stuff: List[Int])

```

The `ExtractionContext` object created in the example above is used by the `scala-schema` extraction mechanism to cache
some information to make subsequent extractions faster. Hence it makes sense to store this object in a variable.

More examples in this [test](https://github.com/Opetushallitus/scala-schema/blob/scala-2.12/src/test/scala/fi/oph/scalaschema/ValidationAndExtractionTest.scala)

### Serialization

Use your schema to serialize your Scala objects into JSON. This is more efficient than using then json4s serialization, because we're
using a preprocessed schema.

```scala
case class Zoo(animals: List[Animal])
case class Animal(name: String, age: Int)

object SerializationExample extends App {
  val context = SerializationContext(SchemaFactory.default)
  val zoo = Zoo(List(Animal("giraffe", 23)))
  val serialized: JValue = Serializer.serialize(zoo, context)
  val stringValue: String = JsonMethods.pretty(serialized)
  println(stringValue)
}
```

You can also apply custom processing to object fields:

```
object CustomSerializationExample extends App {
  def hideAge(schema: ClassSchema, property: Property) = if (property.key == "age") Nil else List(property)
  val context = SerializationContext(SchemaFactory.default, hideAge)
  println(JsonMethods.pretty(Serializer.serialize(zoo, context)))
}
```

In the above example, all fields with the name "age" are hidden. More examples in this [test](https://github.com/Opetushallitus/scala-schema/blob/scala-2.12/src/test/scala/fi/oph/scalaschema/SerializationSpec.scala).

### Schemas and Factories

Now that you've read this far, I'll share some thoughts on schemas and factories.

A `Schema` represents your object model and can be exported as a JSON schema as described above. Schemas are typically created
automatically from your case classes using a `SchemaFactory`. As shown above, you can use annotations to customize how a schema is created,
and also pass information about your custom annotations to your `SchemaFactory`. 

The factory will *cache the created schemas* so that
subsequent requests for a certain schema will be super fast. Therefore you should store your schema factory in a variable, 
but you don't need to store the individual schemas.

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
    <version>2.9.0_2.12</version>
  </dependency>
</dependencies>
```

#### SBT

Add Jitpack.io resolver:

    resolvers += "jitpack" at "https://jitpack.io",

Then add scala-schema as dependency (use appropriate scala version suffix as below)

    libraryDependencies += "com.github.Opetushallitus" % "scala-schema" % "2.9.0_2.12"

### TODO

- Support case classes with type parameters
- Improve error messages of SchemaFactory: include path in all error messages
