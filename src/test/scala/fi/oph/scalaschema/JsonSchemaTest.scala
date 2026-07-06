package fi.oph.scalaschema

import com.github.fge.jsonschema.core.report.ListReportProvider
import com.github.fge.jsonschema.core.report.LogLevel.{ERROR, FATAL}
import com.github.fge.jsonschema.main.{JsonSchemaFactory, JsonValidator}
import fi.oph.scalaschema.TestHelpers.schemaOf
import fi.oph.scalaschema.annotation.{Description, EnumValue, SkipSerialization}
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods.asJsonNode
import org.json4s.jackson._
import org.json4s.{JArray, JValue}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.runtime.universe.TypeTag

class JsonSchemaTest extends AnyFreeSpec with Matchers {
  "Simple example" - {
    val schema = SchemaFactory.default.createSchema(classOf[TestClass])
    val expectedClassSchema = ClassSchema("fi.oph.scalaschema.TestClass", List(
      Property("name", StringSchema()),
      Property("stuff", ListSchema(NumberSchema(classOf[Int]))))
    )

    "Schema object model generation" in {
      schema should equal(expectedClassSchema)
    }
    "Schema for list type" in {
      val schema = SchemaFactory.default.createSchema[List[TestClass]]
      schema should equal(ListSchema(expectedClassSchema))
    }
    "JSON schema generation" in {
      JsonMethods.compact(schema.toJson) should equal("""{"type":"object","properties":{"name":{"type":"string","minLength":1},"stuff":{"type":"array","items":{"type":"number"}}},"id":"#testclass","additionalProperties":false,"title":"Test class","required":["name","stuff"]}""")
    }
  }

  "Features" - {
    "Required (normal) fields" in {
      jsonSchemaOf(classOf[RequiredFields]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#requiredfields","additionalProperties":false,"title":"Required fields","required":["field"]}""")
    }
    "Optional fields" - {
      "Option[A] is treated as non-required field" in {
        jsonSchemaOf(classOf[OptionalFields]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#optionalfields","additionalProperties":false,"title":"Optional fields"}""")
      }
      "Some[A] is treated as required field" in {
        jsonSchemaOf(classOf[SomeFields]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#somefields","additionalProperties":false,"title":"Some fields","required":["field"]}""")
      }
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
        jsonSchemaPropertiesOf(classOf[Dates]) should equal("""{"a":{"type":"string","format":"date"},"b":{"type":"string","format":"date"},"c":{"type":"string","format":"date"},"d":{"type":"string","format":"date"},"e":{"type":"string","format":"date"},"f":{"type":"string","format":"date"}}""")
      }
    }
    "List-like fields" - {
      "List" in {
        jsonSchemaPropertiesOf(classOf[Lists]) should equal("""{"things":{"type":"array","items":{"type":"number"}}}""")
      }
      "Seq" in {
        jsonSchemaPropertiesOf(classOf[Seqs]) should equal("""{"things":{"type":"array","items":{"type":"number"}}}""")
      }
      "Array" in {
        jsonSchemaPropertiesOf(classOf[Arrays]) should equal("""{"things":{"type":"array","items":{"type":"number"}}}""")
      }
      "List accepting single value as array" in {
        jsonSchemaPropertiesOf(classOf[ListsWithSingleValueAsArray]) should equal("""{"things":{"type":"array","items":{"type":"number"},"description":"(when deserializing also accepts a single value)"}}""")
      }
    }
    "Maps" in {
      jsonSchemaPropertiesOf(classOf[Maps]) should equal("""{"things":{"type":"object","patternProperties":{".*":{"type":"number"}}}}""")
    }
    "Objects (uses definitions)" in {
      jsonSchemaOf(classOf[Objects]) should equal("""{"type":"object","properties":{"x":{"$ref":"#/definitions/strings"}},"id":"#objects","additionalProperties":false,"title":"Objects","required":["x"],"definitions":{"strings":{"type":"object","properties":{"s":{"type":"string","minLength":1}},"id":"#strings","additionalProperties":false,"title":"Strings","required":["s"]}}}""")
    }
    "Traits" - {
      "finds implementations in same package, creates anyOf schema" in {
        jsonSchemaOf(classOf[Traits]) should equal("""{"anyOf":[{"$ref":"#/definitions/impla"},{"$ref":"#/definitions/implb"}],"definitions":{"impla":{"type":"object","properties":{},"id":"#impla","additionalProperties":false,"title":"Impl a"},"implb":{"type":"object","properties":{},"id":"#implb","additionalProperties":false,"title":"Impl b"}}}""")
      }
      "works for fields" in {
        jsonSchemaOf(classOf[TraitsInFields]) should equal("""{"type":"object","properties":{"field":{"$ref":"#/definitions/traits"}},"id":"#traitsinfields","additionalProperties":false,"title":"Traits in fields","required":["field"],"definitions":{"impla":{"type":"object","properties":{},"id":"#impla","additionalProperties":false,"title":"Impl a"},"implb":{"type":"object","properties":{},"id":"#implb","additionalProperties":false,"title":"Impl b"},"traits":{"anyOf":[{"$ref":"#/definitions/impla"},{"$ref":"#/definitions/implb"}]}}}""")
      }
    }
    "Fields requiring Scala name encoding" in {
      jsonSchemaPropertiesOf(classOf[ScalaNameEncoding]) should equal("""{"@Foo":{"type":"string","minLength":1},"type":{"type":"string","minLength":1}}""")
    }
    "JValues" - {
      "JValue" in {
        jsonSchemaOf[JValue] should equal("""{}""")
      }
      "JObject" in {
        jsonSchemaOf[JObject] should equal("""{"type":"object"}""")
      }
      "JArray" in {
        jsonSchemaOf[JArray] should equal("""{"type":"array"}""")
      }
    }
    "Specialized schema -> no #id" in {
      jsonSchemaOf(schemaOf(classOf[RequiredFields]).asInstanceOf[ClassSchema].copy(specialized = true)) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"additionalProperties":false,"title":"Required fields","required":["field"]}""")
    }
    "Annotations" - {
      "@DefaultValue" - {
        "Fields with @DefaultValue are treated as non-required" in {
          jsonSchemaOf(classOf[BooleansWithDefault]) should equal("""{"type":"object","properties":{"field":{"type":"boolean","description":"(default value: true)"}},"id":"#booleanswithdefault","additionalProperties":false,"title":"Booleans with default"}""")
        }
      }
      "@Description" - {
        "for case class" in {
          jsonSchemaOf(classOf[WithDescription]) should equal("""{"type":"object","properties":{},"id":"#withdescription","additionalProperties":false,"title":"With description","description":"Boom boom boom"}""")
        }
        "for field" in {
          jsonSchemaOf(classOf[FieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"$ref":"#/definitions/withdescription","description":"Pow pow pow. Boom boom boom"}},"id":"#fieldwithdescription","additionalProperties":false,"title":"Field with description","required":["field"],"definitions":{"withdescription":{"type":"object","properties":{},"id":"#withdescription","additionalProperties":false,"title":"With description","description":"Boom boom boom"}}}""")
        }
        "for optional field" in {
          jsonSchemaOf(classOf[OptionalFieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"$ref":"#/definitions/withdescription","description":"Pow pow pow. Boom boom boom"}},"id":"#optionalfieldwithdescription","additionalProperties":false,"title":"Optional field with description","definitions":{"withdescription":{"type":"object","properties":{},"id":"#withdescription","additionalProperties":false,"title":"With description","description":"Boom boom boom"}}}""")
        }
        "for list field" in {
          jsonSchemaOf(classOf[ListFieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"type":"array","items":{"$ref":"#/definitions/withdescription","description":"Boom boom boom"},"description":"Pow pow pow. Boom boom boom"}},"id":"#listfieldwithdescription","additionalProperties":false,"title":"List field with description","required":["field"],"definitions":{"withdescription":{"type":"object","properties":{},"id":"#withdescription","additionalProperties":false,"title":"With description","description":"Boom boom boom"}}}""")
        }
        "for case class extending a trait" in {
          jsonSchemaOf(classOf[WithTraitWithFieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"type":"string","minLength":1,"description":"Boom boom boom"}},"id":"#withtraitwithfieldwithdescription","additionalProperties":false,"title":"With trait with field with description","required":["field"],"description":"Trait description. Class description"}""")
        }
        "for field of a class that implements a trait" in {
          jsonSchemaOf(classOf[WithClassWithDescription]) should equal("""{"type":"object","properties":{"field":{"$ref":"#/definitions/classwithdescription","description":"Trait description. Class description"}},"id":"#withclasswithdescription","additionalProperties":false,"title":"With class with description","required":["field"],"definitions":{"classwithdescription":{"type":"object","properties":{},"id":"#classwithdescription","additionalProperties":false,"title":"Class with description","description":"Trait description. Class description"}}}""")
        }
        "for trait in anyOf schema" in {
          jsonSchemaOf(classOf[TraitsWithDescription]) should equal("""{"anyOf":[{"$ref":"#/definitions/implc"},{"$ref":"#/definitions/impld"}],"definitions":{"implc":{"type":"object","properties":{},"id":"#implc","additionalProperties":false,"title":"Impl c","description":"common description"},"impld":{"type":"object","properties":{},"id":"#impld","additionalProperties":false,"title":"Impl d","description":"common description"}},"description":"common description"}""")
        }
        "for field with anyOf schema" in {
          jsonSchemaOf(classOf[WithTraitFieldWithDescription]) should equal("""{"type":"object","properties":{"field":{"$ref":"#/definitions/traitswithdescription","description":"common description"}},"id":"#withtraitfieldwithdescription","additionalProperties":false,"title":"With trait field with description","required":["field"],"definitions":{"implc":{"type":"object","properties":{},"id":"#implc","additionalProperties":false,"title":"Impl c","description":"common description"},"impld":{"type":"object","properties":{},"id":"#impld","additionalProperties":false,"title":"Impl d","description":"common description"},"traitswithdescription":{"anyOf":[{"$ref":"#/definitions/implc"},{"$ref":"#/definitions/impld"}],"description":"common description"}}}""")
        }
      }
      "@MinItems, @MaxItems" in {
        jsonSchemaPropertiesOf(classOf[WithMaxMinItems]) should equal("""{"stuff":{"type":"array","items":{"type":"number"},"minItems":1,"description":"(Minimum number of items: 1). (Maximum number of items: 2)","maxItems":2}}""")
      }
      "@MinValue, @MaxValue" in {
        jsonSchemaPropertiesOf(classOf[WithMaxMinValue]) should equal("""{"value":{"type":"number","minimum":1.0,"description":"(Minimum value: 1.0). (Maximum value: 2.0)","maximum":2.0}}""")
      }
      "@MinValueExclusive, @MaxValueExclusive" in {
        jsonSchemaPropertiesOf(classOf[WithMaxMinValueExclusive]) should equal("""{"value":{"type":"number","minimum":1.0,"exclusiveMinimum":true,"description":"(Minimum value: 1.0 exclusive). (Maximum value: 2.0 exclusive)","maximum":2.0,"exclusiveMaximum":true}}""")
      }
      "@RegularExpression" in {
        jsonSchemaPropertiesOf(classOf[WithRegEx]) should equal("""{"date":{"type":"string","minLength":1,"pattern":"^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$","description":"(Format: ^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$)"}}""")
      }
      "@SyntheticProperty" - {
        "for method in case class" in {
          jsonSchemaOf(classOf[WithSyntheticProperties]) should equal("""{"type":"object","properties":{"field1":{"type":"boolean"},"field2":{"type":"array","items":{"type":"boolean"}}},"id":"#withsyntheticproperties","additionalProperties":false,"title":"With synthetic properties"}""")
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
      "@ComputedProperty" - {
        "is omitted by default from owner schema" in {
          verifyProperties(classSchemaOf[LeafComputedProperty], "value")
        }
        "includes only the middle computed property selected by @IncludeComputedProperty" in {
          val rootSchema = classSchemaOf[RootWithMiddleComputedProperty]
          val middleSchema = findSchemaForClass(rootSchema, classOf[MiddleComputedProperty])
          val leafSchema = findSchemaForClass(rootSchema, classOf[LeafComputedProperty])

          verifyProperties(rootSchema, "middle")
          verifyProperties(middleSchema, "leaf", "middleComputedValue")
          verifyProperties(leafSchema, "value")

          verifyJsonPropertyKeys(rootSchema.toJson \ "properties", "middle")
          verifyJsonPropertyKeys(rootSchema.toJson \ "definitions" \ middleSchema.simpleName \ "properties", "leaf", "middleComputedValue")
          verifyJsonPropertyKeys(rootSchema.toJson \ "definitions" \ leafSchema.simpleName \ "properties", "value")
          verifyComputedStringProperty(middleSchema, "middleComputedValue")
        }
        "includes root and leaf computed properties selected by @IncludeComputedProperty" in {
          val rootSchema = classSchemaOf[RootWithRootAndLeafComputedProperties]
          val middleSchema = findSchemaForClass(rootSchema, classOf[MiddleComputedProperty])
          val leafSchema = findSchemaForClass(rootSchema, classOf[LeafComputedProperty])

          verifyProperties(rootSchema, "middle", "rootComputedValue")
          verifyProperties(middleSchema, "leaf")
          verifyProperties(leafSchema, "value", "leafComputedValue")

          verifyJsonPropertyKeys(rootSchema.toJson \ "properties", "middle", "rootComputedValue")
          verifyJsonPropertyKeys(rootSchema.toJson \ "definitions" \ middleSchema.simpleName \ "properties", "leaf")
          verifyJsonPropertyKeys(rootSchema.toJson \ "definitions" \ leafSchema.simpleName \ "properties", "value", "leafComputedValue")
          verifyComputedStringProperty(rootSchema, "rootComputedValue")
          verifyComputedStringProperty(leafSchema, "leafComputedValue")
        }
        "root-aware resolve preserves @IncludeComputedProperty additions in referenced schemas" in {
          val factory = SchemaFactory()
          val rootAndLeafSchema = factory.createSchema(classOf[RootWithRootAndLeafComputedProperties]).asInstanceOf[ClassSchema]
          val middleOnlySchema = factory.createSchema(classOf[RootWithMiddleComputedProperty]).asInstanceOf[ClassSchema]
          val middleRef = ClassRefSchema(classOf[MiddleComputedProperty].getName, Nil)
          val leafRef = ClassRefSchema(classOf[LeafComputedProperty].getName, Nil)

          val rootAndLeafAwareMiddleSchema = middleRef.resolve(factory, rootAndLeafSchema).asInstanceOf[ClassSchema]
          val rootAndLeafAwareLeafSchema = leafRef.resolve(factory, rootAndLeafSchema).asInstanceOf[ClassSchema]
          val middleOnlyAwareMiddleSchema = middleRef.resolve(factory, middleOnlySchema).asInstanceOf[ClassSchema]
          val middleOnlyAwareLeafSchema = leafRef.resolve(factory, middleOnlySchema).asInstanceOf[ClassSchema]
          val rootBlindMiddleSchema = middleRef.resolve(factory).asInstanceOf[ClassSchema]
          val rootBlindLeafSchema = leafRef.resolve(factory).asInstanceOf[ClassSchema]

          verifyProperties(rootAndLeafAwareMiddleSchema, "leaf")
          verifyProperties(rootAndLeafAwareLeafSchema, "value", "leafComputedValue")
          verifyProperties(middleOnlyAwareMiddleSchema, "leaf", "middleComputedValue")
          verifyProperties(middleOnlyAwareLeafSchema, "value")
          verifyProperties(rootBlindMiddleSchema, "leaf")
          verifyProperties(rootBlindLeafSchema, "value")
        }
      }
      "@EnumValue" - {
        "for strings and optional strings" in {
          jsonSchemaOf(classOf[WithEnumValue]) should equal("""{"type":"object","properties":{"a":{"type":"string","enum":["a"],"minLength":1},"b":{"type":"string","enum":["b"],"minLength":1},"c":{"type":"array","items":{"type":"string","enum":["c"],"minLength":1}}},"id":"#withenumvalue","additionalProperties":false,"title":"With enum value","required":["a","c"]}""")
        }

        "fails with wrong type" in {
          intercept[ClassCastException](EnumValue.addEnumValues(StringSchema(), List(true)))
        }
      }

      "@Flatten" - {
        "flattens the schema of a single-field case class" in {
          jsonSchemaOf[FlattenedNumber] should equal("""{"type":"number"}""")
        }
        "fails when case class has more than 1 field" in {
          intercept[RuntimeException](jsonSchemaOf[Flattened2Fields])
        }
      }


      "@ReadFlattened" - {
        "Creates an AnyOf schema that allows a flat version" in {
          jsonSchemaOf[ReadableFromString] should equal("""{"anyOf":[{"type":"object","properties":{"value":{"type":"string","enum":["hello"],"minLength":1},"description":{"type":"string","minLength":1}},"id":"#readablefromstring","additionalProperties":false,"title":"Readable from string","required":["value"]},{"type":"string","enum":["hello"],"minLength":1}]}""")
        }
        "fails when case class has more than 1 required field" in {
          intercept[RuntimeException](jsonSchemaOf[ReadableFromTwoStrings])
        }
      }

      "@SkipSerialization" - {
        "fields annotated with @SkipSerialization should be omitted from generated JSON Schema" in {
          val schema = SchemaFactory.default.createSchema(classOf[WithSkipSerialization])
          val json = JsonMethods.compact(schema.toJson)
          json should equal("""{"type":"object","properties":{"visible":{"type":"string","minLength":1}},"id":"#withskipserialization","additionalProperties":false,"title":"With skip serialization","required":["visible"]}""")
        }
      }

      "Custom metadata annotations" - {
        "Simple case" in {
          jsonSchemaOf(SchemaFactory.default.createSchema[CustomAnnotated]) should equal("""{"type":"object","properties":{},"id":"#customannotated","additionalProperties":false,"title":"Custom annotated","description":"These numbers: 1,2,3"}""")
        }
        "Unsupported annotations are ignored" in {
          jsonSchemaOf(SchemaFactory.default.createSchema[AnnotatedWithUnsupportedAnnotation]) should equal("""{"type":"object","properties":{},"id":"#annotatedwithunsupportedannotation","additionalProperties":false,"title":"Annotated with unsupported annotation","description":"Supported annotation"}""")
        }
        "Transforming property schemas to other schemas" in {
          jsonSchemaOf(SchemaFactory.default.createSchema[MadlyAnnotated]) should equal("""{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#madlyannotated","additionalProperties":false,"title":"Madly annotated","required":["field"]}""")
        }
      }
    }

    "Title" - {
      "CamelCase to words" in {
        ClassRefSchema("com.foo.CamelCase", Nil).title should equal("Camel case")
      }

      "Lodash (_) to dash (-)" in {
        ClassRefSchema("foo.bar.Foo_Bar", Nil).title should equal("Foo-bar")
      }

      "Title annotation" in {
        jsonSchemaOf(classOf[WithTitle]) should equal("""{"type":"object","properties":{},"id":"#withtitle","additionalProperties":false,"title":"Custom title"}""")
      }
    }

    "Moving definitions to top level" - {
      "Happens automatically for nested structures" in {
        jsonSchemaOf(classOf[NestedDefinitions]) should equal("""{"type":"object","properties":{"x":{"$ref":"#/definitions/objects"}},"id":"#nesteddefinitions","additionalProperties":false,"title":"Nested definitions","required":["x"],"definitions":{"objects":{"type":"object","properties":{"x":{"$ref":"#/definitions/strings"}},"id":"#objects","additionalProperties":false,"title":"Objects","required":["x"]},"strings":{"type":"object","properties":{"s":{"type":"string","minLength":1}},"id":"#strings","additionalProperties":false,"title":"Strings","required":["s"]}}}""")
      }
      "Can be performed after creation for artesanal schemas" in {
        val definitions: List[SchemaWithClassName] = List(schemaOf(classOf[NestedDefinitions]), AnyOfSchema(List(schemaOf(classOf[Booleans])), "someanyof", Nil, List(schemaOf(classOf[NestedDefinitions]))))
        val schema = ClassSchema("test", List(Property("testprop", NumberSchema(classOf[Int]))), Nil, definitions).moveDefinitionsToTopLevel
        jsonSchemaOf(schema) should equal("""{"type":"object","properties":{"testprop":{"type":"number"}},"id":"#test","additionalProperties":false,"title":"Test","required":["testprop"],"definitions":{"nesteddefinitions":{"type":"object","properties":{"x":{"$ref":"#/definitions/objects"}},"id":"#nesteddefinitions","additionalProperties":false,"title":"Nested definitions","required":["x"]},"objects":{"type":"object","properties":{"x":{"$ref":"#/definitions/strings"}},"id":"#objects","additionalProperties":false,"title":"Objects","required":["x"]},"strings":{"type":"object","properties":{"s":{"type":"string","minLength":1}},"id":"#strings","additionalProperties":false,"title":"Strings","required":["s"]},"someanyof":{"anyOf":[{"type":"object","properties":{"field":{"type":"boolean"}},"id":"#booleans","additionalProperties":false,"title":"Booleans","required":["field"]}]}}}""")
      }
      "Can be performed after creation for AnyOf schemas" in {
        val schema = AnyOfSchema(List(schemaOf(classOf[NestedDefinitions])), "testing", Nil).moveDefinitionsToTopLevel
        jsonSchemaOf(schema) should equal("""{"anyOf":[{"type":"object","properties":{"x":{"$ref":"#/definitions/objects"}},"id":"#nesteddefinitions","additionalProperties":false,"title":"Nested definitions","required":["x"]}],"definitions":{"objects":{"type":"object","properties":{"x":{"$ref":"#/definitions/strings"}},"id":"#objects","additionalProperties":false,"title":"Objects","required":["x"]},"strings":{"type":"object","properties":{"s":{"type":"string","minLength":1}},"id":"#strings","additionalProperties":false,"title":"Strings","required":["s"]}}}""")
      }
    }
  }
  def jsonSchemaOf[T : TypeTag]: String = jsonSchemaOf(SchemaFactory.default.createSchema[T])
  def jsonSchemaOf(c: Class[_]): String = jsonSchemaOf(schemaOf(c))
  def jsonSchemaOf(s: Schema): String = {
    val schemaJson = s.toJson
    // Just check that the created schema is a valid JSON schema, ignore validation results
    jsonSchemaFactory.getJsonSchema(asJsonNode(SchemaToJson.toJsonSchema(s))).validate(asJsonNode(JObject()))
    JsonMethods.compact(schemaJson)
  }
  def jsonSchemaPropertiesOf(c: Class[_]) = JsonMethods.compact(SchemaFactory.default.createSchema(c).toJson \\ "properties")

  private def classSchemaOf[T : TypeTag]: ClassSchema =
    SchemaFactory().createSchema[T].asInstanceOf[ClassSchema]

  private def findSchemaForClass(rootSchema: ClassSchema, c: Class[_]): ClassSchema =
    rootSchema.definitions.find(_.appliesToClass(c)).get.asInstanceOf[ClassSchema]

  private def verifyProperties(schema: ClassSchema, keys: String*): Unit =
    schema.properties.map(_.key) should equal(keys.toList)

  private def verifyJsonPropertyKeys(propertiesJson: JValue, keys: String*): Unit =
    propertiesJson match {
      case JObject(properties) => properties.map(_._1) should equal(keys.toList)
      case other => fail(s"Expected JSON object properties, got $other")
    }

  private def verifyComputedStringProperty(schema: ClassSchema, key: String): Unit = {
    val computedProperty = schema.properties.find(_.key == key).get

    computedProperty.synthetic should equal(true)
    computedProperty.computed should equal(true)
    computedProperty.schema should equal(OptionalSchema(StringSchema()))
  }

  private lazy val jsonSchemaFactory = JsonSchemaFactory.newBuilder.setReportProvider(new ListReportProvider(ERROR, FATAL)).freeze()
  private lazy val validator: JsonValidator = JsonSchemaFactory.byDefault.getValidator
}

case class CustomAnnotation(numbers: List[Int]) extends Metadata {
  override def appendMetadataToJsonSchema(obj: JObject): JObject = appendToDescription(obj, s"These numbers: ${numbers.mkString(",")}")
}

@CustomAnnotation(List(1, 2, 3))
case class CustomAnnotated()

@UnsupportedTestAnnotation
@Description("Supported annotation")
case class AnnotatedWithUnsupportedAnnotation()

case class MadAnnotation() extends Metadata {
  override def applyMetadata(x: ObjectWithMetadata[_], schemaFactory: SchemaFactory): ObjectWithMetadata[_] = x match {
    case p: Property => p.copy(schema = BooleanSchema())
  }
  def appendMetadataToJsonSchema(obj: JObject): JObject = obj
}

case class MadlyAnnotated(@MadAnnotation field: String)
