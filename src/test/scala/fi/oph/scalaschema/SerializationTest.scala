package fi.oph.scalaschema

import fi.oph.scalaschema.annotation.SkipSerialization

import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.Date
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JValue
import org.json4s.JsonAST.{JArray, JNull, JObject, JString}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.runtime.{universe => ru}

class SerializationTest extends AnyFreeSpec with Matchers {
  "strings" - {
    testSerialization(Strings("a"), """{"s":"a"}""")
  }
  "numbers" in {
    testSerialization(Numbers(1, 1L, 0.4f, 1.1), """{"a":1,"b":1,"c":0.4000000059604645,"d":1.1}""")
  }

  "StringOptions" in {
    testSerialization(StringOptions(None), """{}""")
    testSerialization(StringOptions(Some("Hello")), """{"value":"Hello"}""")
  }

  "traits" in {
    testSerialization(ThingContainingTrait(Impl1("hello")), """{"x":{"x":"hello"}}""")
  }

  "dates" in {
    val dates = Dates(
      LocalDate.parse("2015-12-30"),
      ZonedDateTime.parse("1987-01-23T00:33:23Z"),
      Date.from(java.time.ZonedDateTime.parse("1977-03-13T13:42:11Z").toInstant),
      Timestamp.from(java.time.ZonedDateTime.parse("2007-08-23T10:43:21Z").toInstant),
      ISODateTimeFormat.dateTimeParser.withZoneUTC.parseDateTime("2017-09-13T12:43:21Z"),
      LocalDateTime.parse("2017-09-13T12:43:21")
    )
    testSerialization(dates, """{"a":"2015-12-30","b":"1987-01-23T00:33:23Z","c":"1977-03-13T13:42:11Z","d":"2007-08-23T10:43:21Z","e":"2017-09-13T12:43:21Z","f":"2017-09-13T12:43:21"}""")
  }

  "LocalDateTime can be serialized as root value" in {
    testSerialization(
      LocalDateTime.parse("2017-09-13T12:43:21"),
      """"2017-09-13T12:43:21"""",
      SerializationContext(SchemaFactory())
    )
  }

  "booleans" in {
    testSerialization(Booleans(true), """{"field":true}""")
  }

  "lists" - {
    "List" in {
      testSerialization(Lists(List(1)), """{"things":[1]}""")
    }
    "Seq/Vector" in {
      testSerialization(Seqs(Vector(1)), """{"things":[1]}""")
    }
    "Array" in {
      testSerialization(Arrays(Array(1)), """{"things":[1]}""")
    }
  }

  "maps" in {
    testSerialization(Maps(Map("a" -> 1)), """{"things":{"a":1}}""")
  }

  "options" - {
    "when value exists" in {
      testSerialization(OptionalFields(Some(true)), """{"field":true}""")
    }
    "with default settings, empty options are omitted" in {
      testSerialization(OptionalFields(None), """{}""")
    }
    "with omitEmptyFields = false, empty options are output as nulls" in {
      testSerialization(OptionalFields(None), """{"field":null}""", defaultContext.copy(omitEmptyFields = false))
    }
  }

  "synthetic properties" in {
    testSerialization(WithSyntheticProperties(), """{"field1":true,"field2":[true]}""")
  }

  "overridden synthetic properties" in {
    testSerialization(WithOverriddenSyntheticProperties(false), """{"field":false}""")
  }

  "computed properties" - {
    "root and leaf computed properties are serialized when root schema includes them with @IncludeComputedProperty" in {
      testSerialization(
        rootWithRootAndLeafComputedProperties("leaf-value"),
        """{"middle":{"leaf":{"value":"leaf-value","leafComputedValue":"leaf-computed-value"}},"rootComputedValue":"root-computed-value"}""",
        SerializationContext(SchemaFactory())
      )
    }

    "only middle computed property is serialized when it is the only root schema @IncludeComputedProperty" in {
      testSerialization(
        rootWithMiddleComputedProperty("leaf-value"),
        """{"middle":{"leaf":{"value":"leaf-value"},"middleComputedValue":"middle-computed-value"}}""",
        SerializationContext(SchemaFactory())
      )
    }

    "computed properties are not serialized when no @IncludeComputedProperty is resolved from the root schema" in {
      testSerialization(
        LeafComputedProperty("leaf-value"),
        """{"value":"leaf-value"}""",
        SerializationContext(SchemaFactory())
      )
    }

    "only path-specific computed property is serialized when @IncludeComputedProperty has a dotted suffix path" in {
      testSerialization(
        RootWithTwoComputedOwnerPaths(ComputedOwner("included-value"), ComputedOwner("not-included-value")),
        """{"included":{"value":"included-value","computedValue":"computed-value"},"notIncluded":{"value":"not-included-value"}}""",
        SerializationContext(SchemaFactory())
      )
    }

    "computed properties can be skipped by property processor" in {
      def skipComputedProperties(s: ClassSchema, p: Property): List[Property] =
        if (p.computed) Nil else List(p)

      testSerialization(
        rootWithRootAndLeafComputedProperties("leaf-value"),
        """{"middle":{"leaf":{"value":"leaf-value"}}}""",
        SerializationContext(SchemaFactory(), propertyProcessor = skipComputedProperties)
      )
    }
  }

  "empty optional" in {
    val json = Serializer.serialize(WithOptionalDiscriminator("name", None), defaultContext)
    json should equal(JObject("name" -> JString("name")))
  }

  "JValues" - {
    "JValue field" in {
      testSerialization(WithJValue(JString("hello")), """{"x":"hello"}""")
    }

    "JValue null field" in {
      testSerialization(WithJValue(JNull), """{"x":null}""")
    }

    "JValue" in {
      testSerialization(JString("hello").asInstanceOf[JValue], """"hello"""")
    }

    "JObject" in {
      testSerialization(JObject(), """{}""")
    }

    "JArray" in {
      testSerialization(JArray(List()), """[]""")
    }
  }

  "custom field filtering" in {
    def skipOtherThanA(s: ClassSchema, p: Property) = if (p.key == "a") List(p) else Nil
    testSerialization(Numbers(1, 1L, 0.4f, 1.1), """{"a":1}""", context = SerializationContext(SchemaFactory.default, propertyProcessor = skipOtherThanA))
  }

  "@Flatten annotation" in {
    testSerialization(FlattenedNumber(1), """1""")
  }

  "@ReadFlattened annotation" in {
    testSerialization(ReadableFromString("hello", None), """{"value":"hello"}""")
  }

  "Scala name encoding" in {
    testSerialization(ScalaNameEncoding("hello", "bar"), """{"@Foo":"hello","type":"bar"}""")
  }

  "@SkipSerialization annotation" - {
    "should exclude annotated fields from serialization" in {
      val value = WithSkipSerialization("shown", Some("secret"))
      testSerialization(value, """{"visible":"shown"}""")
    }

    "should omit annotated fields even if null" in {
      val value = WithSkipSerialization("shown", None)
      testSerialization(value, """{"visible":"shown"}""")
    }
  }

  def testSerialization[T](x: T, expected: String, context: SerializationContext = defaultContext)(implicit tag: ru.TypeTag[T]) = {
    val jValue = Serializer.serialize(x, context)
    org.json4s.jackson.JsonMethods.compact(jValue) should equal(expected)
  }

  private def rootWithRootAndLeafComputedProperties(value: String) =
    RootWithRootAndLeafComputedProperties(MiddleComputedProperty(LeafComputedProperty(value)))

  private def rootWithMiddleComputedProperty(value: String) =
    RootWithMiddleComputedProperty(MiddleComputedProperty(LeafComputedProperty(value)))

  private def defaultContext[T] =
    SerializationContext(SchemaFactory.default)
}

case class ThingContainingTrait(x: TraitsWithFields)
trait TraitsWithFields
case class Impl1(x: String) extends TraitsWithFields
case class Impl2(x: Int) extends TraitsWithFields

case class WithSkipSerialization(
  visible: String,
  @SkipSerialization hidden: Option[String]
)
