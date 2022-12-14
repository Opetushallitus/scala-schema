package fi.oph.scalaschema

import fi.oph.scalaschema.extraction.{UnexpectedProperty, ValidationError}
import org.json4s.JString
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.runtime.{universe => ru}

case class NameClass(value: Int)
case class ClassRefTestClass(name: NameClass, stuff: List[NameClass])

case class Nested3(value: Int)
case class Nested2(value: Nested3)
case class Nested1(value: Nested2)
case class MultiLevelRefTestClass(name: Nested1)

class ClassReferenceTest extends AnyFreeSpec with Matchers {
  "Serialization" - {
    "Should include class references, when includeClassReferences parameter is set to 'true'" in {
      val zoo = Zoo(List(Animal("giraffe", 23)))
      testSerialization(zoo, """{"animals":[{"name":"giraffe","age":23,"$class":"fi.oph.scalaschema.Animal"}],"$class":"fi.oph.scalaschema.Zoo"}""", context = SerializationContext(SchemaFactory.default, includeClassReferences = true))
    }
    "Should include class references, when includeClassReferences parameter is set to 'true', for nested classes" in {
      val zoo = MultiLevelRefTestClass(Nested1(Nested2(Nested3(42))))
      testSerialization(zoo, """{"name":{"value":{"value":{"value":42,"$class":"fi.oph.scalaschema.Nested3"},"$class":"fi.oph.scalaschema.Nested2"},"$class":"fi.oph.scalaschema.Nested1"},"$class":"fi.oph.scalaschema.MultiLevelRefTestClass"}""", context = SerializationContext(SchemaFactory.default, includeClassReferences = true))
    }
    "Should not include class references, when includeClassReferences parameter is set to 'false'" in {
      val zoo = Zoo(List(Animal("giraffe", 23)))
      testSerialization(zoo, """{"animals":[{"name":"giraffe","age":23}]}""", context = SerializationContext(SchemaFactory.default))
    }
    "Should not include class references, when includeClassReferences parameter is omitted" in {
      val zoo = Zoo(List(Animal("giraffe", 23)))
      testSerialization(zoo, """{"animals":[{"name":"giraffe","age":23}]}""", context = SerializationContext(SchemaFactory.default, includeClassReferences = false))
    }
  }
  "Deserialization" - {
    "Should not include class references, when extracting JSON object into Scala class" in {
      implicit val context = ExtractionContext(SchemaFactory.default)
      SchemaValidatingExtractor.extract[Zoo]("""{"animals":[{"name":"giraffe","age":23,"$class":"fi.oph.scalaschema.Animal"}],"$class":"fi.oph.scalaschema.Zoo"}""") should equal(Right(Zoo(List(Animal("giraffe", 23)))))
    }
    "Should fail if trying to include class references" in {
      implicit val context = ExtractionContext(SchemaFactory.default, stripClassReferences = false)
      SchemaValidatingExtractor.extract[Zoo]("""{"animals":[{"name":"giraffe","age":23,"$class":"fi.oph.scalaschema.Animal"}],"$class":"fi.oph.scalaschema.Zoo"}""") should equal(Left(List(
        ValidationError("animals.0.$class", JString("fi.oph.scalaschema.Animal"), UnexpectedProperty()),
        ValidationError("$class", JString("fi.oph.scalaschema.Zoo"), UnexpectedProperty())
      )))
    }
  }

  private def testSerialization[T](x: T, expected: String, context: SerializationContext = defaultContext)(implicit tag: ru.TypeTag[T]) = {
    val jValue = Serializer.serialize(x, context)
    org.json4s.jackson.JsonMethods.compact(jValue) should equal(expected)
  }

  private def defaultContext[T] = SerializationContext(SchemaFactory.default)
}