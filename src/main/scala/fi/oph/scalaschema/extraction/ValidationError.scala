package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{EnumValue, SerializableNotWhen, SerializableOnlyWhen}
import org.json4s.JValue

case class ValidationError(path: String, value: JValue, error: ValidationRuleViolation)

sealed trait ValidationRuleViolation
case class MissingProperty(@EnumValue("missingProperty") errorType: String = "missingProperty") extends ValidationRuleViolation
case class UnexpectedProperty(@EnumValue("unexpectedProperty") errorType: String = "unexpectedProperty") extends ValidationRuleViolation
case class UnexpectedType(expectedType: String, @EnumValue("unexpectedType") errorType: String = "unexpectedType") extends ValidationRuleViolation
case class DateFormatMismatch(expectedFormat: String, @EnumValue("dateFormatMismatch") errorType: String = "dateFormatMismatch") extends ValidationRuleViolation
case class EmptyString(@EnumValue("emptyString") errorType: String = "emptyString") extends ValidationRuleViolation
case class RegExMismatch(regex: String, @EnumValue("regularExpressionMismatch") errorType: String = "regularExpressionMismatch") extends ValidationRuleViolation
case class EnumValueMismatch(allowedValues: List[JValue], @EnumValue("enumValueMismatch") errorType: String = "enumValueMismatch") extends ValidationRuleViolation
case class NotAnyOf(allowedAlternatives: Map[String, List[String]], @EnumValue("notAnyOf") errorType: String = "notAnyOf") extends ValidationRuleViolation
case class SmallerThanMinimumValue(minimumValue: Double, @EnumValue("smallerThanMinimumValue") errorType: String = "smallerThanMinimumValue") extends ValidationRuleViolation
case class GreaterThanMaximumValue(maximumValue: Double, @EnumValue("greaterThanMaximumValue") errorType: String = "greaterThanMaximumValue") extends ValidationRuleViolation
case class SmallerThanOrEqualToExclusiveMinimumValue(exclusiveMinimumValue: Double, @EnumValue("smallerThanOrEqualToExclusiveMinimumValue") errorType: String = "smallerThanOrEqualToExclusiveMinimumValue") extends ValidationRuleViolation
case class GreaterThanOrEqualToExclusiveMaximumValue(exclusiveMaximumValue: Double, @EnumValue("greaterThanOrEqualToExclusiveMaximumValue") errorType: String = "greaterThanOrEqualToExclusiveMaximumValue") extends ValidationRuleViolation
case class LessThanMinimumNumberOfItems(minimumItems: Int, @EnumValue("lessThanMinimumNumberOfItems") errorType: String = "lessThanMinimumNumberOfItems") extends ValidationRuleViolation
case class MoreThanMaximumNumberOfItems(maximumItems: Int, @EnumValue("moreThanMaximumNumberOfItems") errorType: String = "moreThanMaximumNumberOfItems") extends ValidationRuleViolation
case class OtherViolation(message: String, @EnumValue("otherViolation") errorType: String = "otherViolation") extends ValidationRuleViolation
case class OnlyWhenMismatch(oneOfMustMatch: List[SerializableOnlyWhen], @EnumValue("onlyWhenMismatch") errorType: String="onlyWhenMismatch") extends ValidationRuleViolation
case class NotWhenMismatch(noneMustMatch: List[SerializableNotWhen], @EnumValue("notWhenMismatch") errorType: String="notWhenMismatch") extends ValidationRuleViolation