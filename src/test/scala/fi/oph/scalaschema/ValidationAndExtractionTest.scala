package fi.oph.scalaschema

import fi.oph.scalaschema.extraction.{ValidationError, _}
import org.json4s.JsonAST.{JInt, JNothing, JObject, JString}
import org.scalatest.{FreeSpec, Matchers}

class ValidationAndExtractionTest extends FreeSpec with Matchers with TestHelpers {
  "Validation and extraction" - {
    "Simple example" - {
      "Extraction" in {
        val testValue = TestClass("name", List(1, 2, 3))
        val json = Json.toJValue(testValue)
        implicit val context = ExtractionContext(schemaOf(classOf[TestClass]))
        SchemaValidatingExtractor.extract[TestClass](json) should equal(Right(testValue))
      }
      "Missing fields validation" in {
        verifyValidation(JObject(), classOf[TestClass], Left(List(
          ValidationError("name",JNothing,MissingProperty()),
          ValidationError("stuff",JNothing,MissingProperty())
        )))
      }
      "Field type validation" in {
        verifyValidation(Map(("name" -> 10), ("stuff", List("a", "b"))), classOf[TestClass], Left(List(
          ValidationError("name",JInt(10),UnexpectedType("string")),
          ValidationError("stuff.0",JString("a"),UnexpectedType("number")),
          ValidationError("stuff.1",JString("b"),UnexpectedType("number"))
        )))
      }
    }
  }

  private def verifyValidation(input: AnyRef, klass: Class[_], expectedResult: Either[List[ValidationError], AnyRef]) = {
    implicit val context = ExtractionContext(schemaOf(klass))
    val json = Json.toJValue(input)
    SchemaValidatingExtractor.extract(json, klass) should equal(expectedResult)
  }
}
