package fi.oph.scalaschema

import fi.oph.scalaschema.annotation.{Discriminator, EnumValue}
import fi.oph.scalaschema.extraction.{ValidationError, _}
import org.json4s.JsonAST._
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
    "Numbers" - {
      "As case class fields" in {
        verifyExtractionRoundTrip(Numbers(1, 1, 1, 1))
      }
      "Including BigDecimal, BigInt" in {
        verifyExtractionRoundTrip(MoreNumbers(1, 1, 1, 1, 1, 1))
      }
      "In lists" in {
        val result = verifyExtractionRoundTrip(MoreNumbersInLists(List(1), List(1), List(1), List(1), List(1), List(1)))
        // Force unboxing to make sure it works (didn't work before correct conversions in NumberExtractor)
        val i: Int = result.i.head
        val f: Float = result.f.head
        val l: Long = result.l.head
        val d: Double = result.d.head
        val bi: BigInt = result.bi.head
        val bd: BigDecimal = result.bd.head
      }
      "Optional" in {
        val result: OptionalNumbers = verifyExtractionRoundTrip(OptionalNumbers(Option(1), Option(1), Option(1), Option(1), Option(1), Option(1)))
        // Force unboxing to make sure it works (didn't work before correct conversions in NumberExtractor)
        val i: Int = result.i.get
        val f: Float = result.f.get
        val l: Long = result.l.get
        val d: Double = result.d.get
        val bi: BigInt = result.bi.get
        val bd: BigDecimal = result.bd.get
      }
    }
    "Synthetic properties" - {
      "Are ignored" in {
        verifyExtractionRoundTrip(WithSyntheticProperties())
        verifyValidation(JObject(), classOf[WithSyntheticProperties], Right(WithSyntheticProperties()))
        verifyValidation(JObject(("field" -> JBool(true))), classOf[WithSyntheticProperties], Right(WithSyntheticProperties()))
      }
    }
    "Traits" - {
      "Decides on appropriate trait implementation automatically if determinable from required fields" in {
        verifyExtractionRoundTrip[EasilyDecidableTrait](NonEmpty("hello"))
        verifyExtractionRoundTrip[EasilyDecidableTrait](Empty())
      }
      "Fails gracefully when implementation is non-decidable" in {
        try {
          verifyExtractionRoundTrip[Traits](ImplA())
          fail("Shouldn't succeed")
        } catch {
          case e: TooManyMatchingCasesException =>
            e.cases.length should equal(2)
            e.path should equal("")
        }
      }
      "Respects @Discriminator" - {
        "Existence of @Discriminator field" in {
          verifyExtractionRoundTrip[DiscriminatorResolving](SomethingElse("hello"))
        }
        "@EnumValue annotation in @Discriminator field" in {
          verifyExtractionRoundTrip[DiscriminatorResolving](TypeA())
          verifyExtractionRoundTrip[DiscriminatorResolving](Type2())
        }
      }
    }
  }

  private def verifyValidation(input: AnyRef, klass: Class[_], expectedResult: Either[List[ValidationError], AnyRef]) = {
    implicit val context = ExtractionContext(schemaOf(klass))
    val json = Json.toJValue(input)
    SchemaValidatingExtractor.extract(json, klass) should equal(expectedResult)
  }

  private def verifyExtractionRoundTrip[T](input: T)(implicit mf: ClassManifest[T]): T = {
    implicit val context = ExtractionContext(schemaOf(mf.runtimeClass))
    val json = Json.toJValue(input)
    val result = SchemaValidatingExtractor.extract[T](json)
    result should equal(Right(input))
    result.right.get
  }
}

trait EasilyDecidableTrait
case class NonEmpty(value: String) extends EasilyDecidableTrait
case class Empty() extends EasilyDecidableTrait


trait DiscriminatorResolving
case class TypeA(
  @Discriminator @EnumValue("a") `type`: String = "a"
) extends DiscriminatorResolving
case class TypeB(
  @Discriminator @EnumValue("b") `type`: String = "b"
) extends DiscriminatorResolving

case class Type1(
  @Discriminator @EnumValue(1) `type`: Int = 1
) extends DiscriminatorResolving

case class Type2(
  @Discriminator @EnumValue(2) `type`: Int = 2
) extends DiscriminatorResolving

case class SomethingElse(
  // Existence of this field used for detection
  @Discriminator field: String
) extends DiscriminatorResolving


case class MoreNumbers(i: Int, f: Float, l: Long, d: Double, bd : BigDecimal, bi: BigInt)
case class MoreNumbersInLists(i: List[Int], f: List[Float], l: List[Long], d: List[Double], bd : List[BigDecimal], bi: List[BigInt])
case class OptionalNumbers(i: Option[Int], f: Option[Float], l: Option[Long], d: Option[Double], bd : Option[BigDecimal], bi: Option[BigInt])