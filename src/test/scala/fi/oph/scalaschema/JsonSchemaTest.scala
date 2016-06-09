package fi.oph.scalaschema

import java.time.LocalDate
import fi.oph.scalaschema.annotation._
import org.json4s.jackson._
import org.scalatest.{FreeSpec, Matchers}

class JsonSchemaTest extends FreeSpec with Matchers {
  val schema = SchemaFactory.default.createSchema(classOf[TestClass])

  "Simple example" - {
    "Schema object model generation" in {
      schema should equal(ClassSchema("fi.oph.scalaschema.TestClass", List(
        Property("name", StringSchema()),
        Property("stuff", ListSchema(NumberSchema())))
      ))
    }
    "JSON schema generation" in {
      JsonMethods.compact(schema.toJson) should equal("""{"type":"object","properties":{"name":{"type":"string","minLength":1},"stuff":{"type":"array","items":{"type":"number"}}},"id":"#testclass","additionalProperties":false,"title":"Test class","required":["name","stuff"]}""")
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
    "Annotations" - {
      "@Description" - {
        "for case class" in {
          jsonSchemaOf(classOf[WithDescription]) should equal("""{"type":"object","properties":{},"id":"#withdescription","additionalProperties":false,"title":"With description","description":"Boom boom boom"}""")
        }
        "for field" in {
          jsonSchemaOf(classOf[FieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"type":"string","minLength":1,"description":"Boom boom boom"}},"id":"#fieldwithdescription","additionalProperties":false,"title":"Field with description","required":["field"]}""")
        }
        "for field in trait" in {
          jsonSchemaOf(classOf[WithTraitWithFieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"type":"string","minLength":1,"description":"Boom boom boom"}},"id":"#withtraitwithfieldwithdescription","additionalProperties":false,"title":"With trait with field with description","required":["field"]}""")
        }
      }
      "@MinItems, @MaxItems" in {
        jsonSchemaPropertiesOf(classOf[WithMaxMinItems]) should equal("""{"stuff":{"type":"array","items":{"type":"number"},"minItems":1,"description":"(Minimum number of items: 1). (Maximum number of items: 2)","maxItems":2}}""")
      }
      "@MinValue, @MaxValue" in {
        jsonSchemaPropertiesOf(classOf[WithMaxMinValue]) should equal("""{"value":{"type":"number","minimum":1.0,"description":"(Minimum value: 1.0). (Maximum value: 2.0)","maximum":2.0}}""")
      }
      "@RegularExpression" in {
        jsonSchemaPropertiesOf(classOf[WithRegEx]) should equal("""{"date":{"type":"string","minLength":1,"pattern":"^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$","description":"(Format: ^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$)"}}""")
      }
      "@SyntheticProperty" - {
        "for method in case class" in {
          jsonSchemaOf(classOf[WithSyntheticProperties]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#withsyntheticproperties","additionalProperties":false,"title":"With synthetic properties"}""")
        }
        "for method in trait" in {
          jsonSchemaOf(classOf[WithTraitWithSyntheticProperties]) should equal("""{"type":"object","properties":{"field":{"type":"boolean","description":"synthetic field"}},"id":"#withtraitwithsyntheticproperties","additionalProperties":false,"title":"With trait with synthetic properties"}""")
        }
        "for complex hierarchy of traits" in {
          jsonSchemaOf(classOf[WithComplexHierarchyOfTraitsWithSyntheticProperties]) should equal("""{"type":"object","properties":{"field":{"type":"boolean","description":"synthetic field"}},"id":"#withcomplexhierarchyoftraitswithsyntheticproperties","additionalProperties":false,"title":"With complex hierarchy of traits with synthetic properties"}""")
        }
        "for method in trait overridden by val" in {
          jsonSchemaOf(classOf[WithOverriddenSyntheticProperties]) should equal("""{"type":"object","properties":{"field":{"type":"boolean","description":"synthetic field"}},"id":"#withoverriddensyntheticproperties","additionalProperties":false,"title":"With overridden synthetic properties","required":["field"]}""")
        }
      }
    }
  }

  def jsonSchemaOf(c: Class[_]) = JsonMethods.compact(SchemaFactory.default.createSchema(c).toJson)
  def jsonSchemaPropertiesOf(c: Class[_]) = JsonMethods.compact(SchemaFactory.default.createSchema(c).toJson \\ "properties")
}

case class RequiredFields(field: Boolean)
case class OptionalFields(field: Option[Boolean])
case class Booleans(field: Boolean)
case class Numbers(a: Int, b: Long, c: Float, d: Double)
case class Strings(s: String)
case class Dates(d: LocalDate)
case class Lists(things: List[Int])
case class Objects(x: Strings)

@Description("Boom boom boom")
case class WithDescription()
case class FieldWithDescription(@Description("Boom boom boom") field: String)
trait TraitWithFieldWithDescription { @Description("Boom boom boom") def field: String }
case class WithTraitWithFieldWithDescription(field: String) extends TraitWithFieldWithDescription
case class WithMaxMinItems(@MinItems(1) @MaxItems(2) stuff: List[Int])
case class WithMaxMinValue(@MinValue(1) @MaxValue(2) value: Int)
case class WithRegEx(@RegularExpression("^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$") date: String)
case class WithSyntheticProperties() {
  @SyntheticProperty
  def field: Boolean = true
}

case class WithTraitWithSyntheticProperties() extends TraitWithSyntheticProperties with OtherTraitWithSyntheticProperties
case class WithComplexHierarchyOfTraitsWithSyntheticProperties() extends SomeSubTrait with OtherSubTrait
case class WithOverriddenSyntheticProperties(override val field: Boolean) extends TraitWithSyntheticProperties with OtherTraitWithSyntheticProperties

trait TraitWithSyntheticProperties {
  @SyntheticProperty
  @Description("synthetic field")
  def field: Boolean = true
}
trait OtherTraitWithSyntheticProperties {
  @SyntheticProperty
  def field: Boolean
}
trait OtherSubTrait extends TraitWithSyntheticProperties {
  def field: Boolean
}
trait SomeSubTrait extends TraitWithSyntheticProperties {
}

sealed trait Traits
case class ImplA() extends Traits
case class ImplB() extends Traits

case class TestClass(name: String, stuff: List[Int])

