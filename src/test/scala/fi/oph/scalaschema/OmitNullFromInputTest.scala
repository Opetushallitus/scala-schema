package fi.oph.scalaschema

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait SomeTrait {
}

case class SomeTraitBranch1(first: String, second: Option[String] = None, third: Option[String] = None) extends SomeTrait {
}

case class SomeTraitBranch2(second: String, third: Option[String] = None) extends SomeTrait {
}

case class SomeTraitBranch3(third: String) extends SomeTrait {
}




class OmitNullFromInputTest extends AnyFreeSpec with Matchers {
  "OmitNullFromInput" - {
    "When JSON contains a null value" - {
      implicit val context = ExtractionContext(SchemaFactory.default, omitNullFromInput = true)
      "Should extract case class SomeTraitBranch1 correctly" in {
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": "Example first"}""") should equal(Right(SomeTraitBranch1("Example first")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": "Example first", "second": null}""") should equal(Right(SomeTraitBranch1("Example first")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": "Example first", "second": null, "third": null}""") should equal(Right(SomeTraitBranch1("Example first")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": "Example first", "second": "Example second", "third": "Example third"}""") should equal(Right(SomeTraitBranch1("Example first",Some("Example second"),Some("Example third"))))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": "Example first", "third": null}""") should equal(Right(SomeTraitBranch1("Example first")))
      }
      "Should extract case class SomeTraitBranch2 correctly" in {
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": null, "second": "Example second", "third": null}""") should equal(Right(SomeTraitBranch2("Example second")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"second": "Example second", "third": null}""") should equal(Right(SomeTraitBranch2("Example second")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": null, "second": "Example second"}""") should equal(Right(SomeTraitBranch2("Example second")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"second": "Example second"}""") should equal(Right(SomeTraitBranch2("Example second")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"second": "Example second", "third": "Example third"}""") should equal(Right(SomeTraitBranch2("Example second", Some("Example third"))))
      }
      "Should extract case class SomeTraitBranch3 correctly" in {
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": null, "second": null, "third": "Example third"}""") should equal(Right(SomeTraitBranch3("Example third")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"first": null, "third": "Example third"}""") should equal(Right(SomeTraitBranch3("Example third")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"second": null, "third": "Example third"}""") should equal(Right(SomeTraitBranch3("Example third")))
        SchemaValidatingExtractor.extract[SomeTrait]("""{"third": "Example third"}""") should equal(Right(SomeTraitBranch3("Example third")))
      }
    }
  }
}