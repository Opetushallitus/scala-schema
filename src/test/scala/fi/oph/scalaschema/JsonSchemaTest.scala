package fi.oph.scalaschema

import java.time.LocalDate

import org.json4s.jackson._
import org.scalatest.{FreeSpec, Matchers}

class JsonSchemaTest extends FreeSpec with Matchers {
  implicit val metadataSupport: List[MetadataSupport[_]] = List()
  private val factory: SchemaFactory = SchemaFactory(metadataSupport)
  val schema = factory.createSchema(classOf[TestClass])

  "Simple example" - {
    "Schema object model generation" in {
      schema should equal(ClassSchema("fi.oph.scalaschema.TestClass", List(
        Property("name", StringSchema()),
        Property("stuff", ListSchema(NumberSchema())))
      ))
    }
    "JSON schema generation" in {
      JsonMethods.compact(SchemaToJson.toJsonSchema(schema)) should equal("""{"type":"object","properties":{"name":{"type":"string","minLength":1},"stuff":{"type":"array","items":{"type":"number"}}},"id":"#testclass","additionalProperties":false,"title":"Test class","required":["name","stuff"]}""")
    }
  }

  "Features" - {
    "Required (normal) fields" in {
      jsonSchemaOf(classOf[RequiredFields]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#requiredfields","additionalProperties":false,"title":"Required fields","required":["field"]}""")
    }
    "Optional fields" in {
      jsonSchemaOf(classOf[OptionalFields]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#optionalfields","additionalProperties":false,"title":"Optional fields"}""")
    }
    "Primitives" - {
      "Booleans" in {
        jsonSchemaPropertiesOf(classOf[Booleans]) should equal("""{"field":{"type":"boolean"}}""")
      }
      "Numbers" in {
        jsonSchemaPropertiesOf(classOf[Numbers]) should equal("""{"a":{"type":"number"},"b":{"type":"number"},"c":{"type":"number"},"d":{"type":"number"}}""")
      }
      "Strings (doesn't allow zero-length)" in {
        jsonSchemaPropertiesOf(classOf[Strings]) should equal("""{"s":{"type":"string","minLength":1}}""")
      }
      "Dates" in {
        jsonSchemaPropertiesOf(classOf[Dates]) should equal("""{"d":{"type":"string","format":"date"}}""")
      }
    }
    "Lists" in {
      jsonSchemaPropertiesOf(classOf[Lists]) should equal("""{"things":{"type":"array","items":{"type":"number"}}}""")
    }
    "Objects (uses definitions)" in {
      jsonSchemaOf(classOf[Objects]) should equal("""{"type":"object","properties":{"x":{"$ref":"#/definitions/strings"}},"id":"#objects","additionalProperties":false,"title":"Objects","required":["x"],"definitions":{"strings":{"type":"object","properties":{"s":{"type":"string","minLength":1}},"id":"#strings","additionalProperties":false,"title":"Strings","required":["s"]}}}""")
    }
    "Traits (finds implementations in same package)" in {
      jsonSchemaOf(classOf[Traits]) should equal("""{"anyOf":[{"type":"object","properties":{},"id":"#impla","additionalProperties":false,"title":"Impl a"},{"type":"object","properties":{},"id":"#implb","additionalProperties":false,"title":"Impl b"}]}""")
    }
  }

  def jsonSchemaOf(c: Class[_]) = JsonMethods.compact(SchemaToJson.toJsonSchema((factory.createSchema(c))))
  def jsonSchemaPropertiesOf(c: Class[_]) = JsonMethods.compact(SchemaToJson.toJsonSchema((factory.createSchema(c))) \\ "properties")
}

case class RequiredFields(field: Boolean)
case class OptionalFields(field: Option[Boolean])
case class Booleans(field: Boolean)
case class Numbers(a: Int, b: Long, c: Float, d: Double)
case class Strings(s: String)
case class Dates(d: LocalDate)
case class Lists(things: List[Int])
case class Objects(x: Strings)

sealed trait Traits
case class ImplA() extends Traits
case class ImplB() extends Traits

case class TestClass(name: String, stuff: List[Int])
