package fi.oph.scalaschema

import java.sql.Timestamp
import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import fi.oph.scalaschema.annotation._
import org.joda.time.DateTime
import org.json4s.JValue

case class RequiredFields(field: Boolean)
case class OptionalFields(field: Option[Boolean])
case class SomeFields(field: Some[Boolean])
case class Booleans(field: Boolean)
case class BooleansWithDefault(@DefaultValue(true) field: Boolean)
case class StringsWithDefault(@DefaultValue("hello") field: String)
case class NumbersWithDefault(@DefaultValue(1) field: Int)
case class Numbers(a: Int, b: Long, c: Float, d: Double)
case class Strings(s: String)
case class Dates(a: LocalDate, b: ZonedDateTime, c: Date, d: Timestamp, e: DateTime)
case class Lists(things: List[Int])
case class Objects(x: Strings)
case class NestedDefinitions(x: Objects)
case class Maps(things: Map[String, Int])

@Description("Boom boom boom")
case class WithDescription()
@Title("Custom title")
case class WithTitle()
case class FieldWithDescription(@Description("Pow pow pow") field: WithDescription)
case class OptionalFieldWithDescription(@Description("Pow pow pow") field: Option[WithDescription])
case class ListFieldWithDescription(@Description("Pow pow pow") field: List[WithDescription])
case class OptionalListFieldWithDescription(@Description("Pow pow pow") field: Option[List[WithDescription]])
@Description("Trait description")
trait TraitWithFieldWithDescription { @Description("Boom boom boom") def field: String }
@Description("Class description")
case class WithTraitWithFieldWithDescription(field: String) extends TraitWithFieldWithDescription
@Description("Trait description")
trait TraitWithDescription
@Description("Class description")
case class ClassWithDescription() extends TraitWithDescription
case class WithClassWithDescription(field: ClassWithDescription)
case class WithMaxMinItems(@MinItems(1) @MaxItems(2) stuff: List[Int])
case class WithMaxMinValue(@MinValue(1) @MaxValue(2) value: Int)
case class WithMaxMinValueExclusive(@MinValueExclusive(1) @MaxValueExclusive(2) value: Int)
case class WithRegEx(@RegularExpression("^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$") date: String)
case class WithSyntheticProperties() {
  @SyntheticProperty
  def field1: Boolean = true
  @SyntheticProperty
  def field2: List[Boolean] = List(true)
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

case class TraitsInFields(field: Traits)

@Description("common description")
sealed trait TraitsWithDescription
case class ImplC() extends TraitsWithDescription
case class ImplD() extends TraitsWithDescription
case class WithTraitFieldWithDescription(field: TraitsWithDescription)

case class TestClass(name: String, stuff: List[Int])

trait TestTrait {
  def name: String
}
case class WithOptionalDiscriminator(
  name: String,
  @Discriminator
  id: Option[String]
) extends TestTrait

trait WithEnumerableFieldsAB {
  @EnumValue("a") def a: String
  @EnumValue("b") def b: Option[String]
}
case class WithEnumValue(@EnumValue("a") a: String, b: Option[String], @EnumValue("c") c: List[String]) extends WithEnumerableFieldsAB
case class WithJValue(x: JValue)