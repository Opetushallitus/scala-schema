package fi.oph.scalaschema

import org.json4s.{JArray, JBool, JDouble, JInt, JNull, JString}
import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

case class Something(val1: String)

class ValueConversionTest extends AnyFreeSpec with Matchers {
  "Value conversion" - {
    "Converts Option correctly" in {
      ValueConversion.anyToJValue(None) should matchPattern {
        case JNull =>
      }
      ValueConversion.anyToJValue(Some("Hello")) should matchPattern {
        case JString("Hello") =>
      }
    }
    "Converts String correctly" in {
      ValueConversion.anyToJValue("Hello") should matchPattern {
        case JString("Hello") =>
      }
    }
    "Converts Number correctly" in {
      val integerExample = ValueConversion.anyToJValue(42)
      val doubleExample = ValueConversion.anyToJValue(42.55)
      inside(integerExample) {
        case JInt(a) => {
          a.toInt should be (42)
        }
      }
      inside(doubleExample) {
        case JDouble(a) => {
          a should be (42.55)
        }
      }
    }
    "Converts Boolean correctly" in {
      val booleanExample1 = ValueConversion.anyToJValue(true)
      booleanExample1 should matchPattern {
        case JBool(true) =>
      }
      val booleanExample2 = ValueConversion.anyToJValue(false)
      booleanExample2 should matchPattern {
        case JBool(false) =>
      }
    }
    "Converts List correctly" in {
      val listExample1 = ValueConversion.anyToJValueWithLists(List(true, false, 22, "Hello"))
      listExample1 should matchPattern {
        case JArray(List(JBool(true), JBool(false), _, JString("Hello"))) =>
      }
      val listExample2 = ValueConversion.anyToJValueWithLists(List("Hello", "World", "123"))
      listExample2 should matchPattern {
        case JArray(List(JString("Hello"), JString("World"), JString("123"))) =>
      }
      val listExample3 = ValueConversion.anyToJValueWithLists(List("Hello", List("World", 456), "123"))
      listExample3 should matchPattern {
        case JArray(List(JString("Hello"), JArray(List(JString("World"), _)), JString("123"))) =>
      }
    }
    "Throws IllegalArgumentException on unknown type" in {
      assertThrows[IllegalArgumentException] {
        ValueConversion.anyToJValue(Something("ShouldThrow"))
      }
    }
  }
}
