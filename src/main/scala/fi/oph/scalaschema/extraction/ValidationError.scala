package fi.oph.scalaschema.extraction

import org.json4s.JValue

case class ValidationError(path: String, value: JValue, error: ValidationRuleViolation)

sealed trait ValidationRuleViolation
case class MissingProperty(errorType: String = "missingProperty") extends ValidationRuleViolation
case class UnwantedProperty(errorType: String = "unwantedProperty") extends ValidationRuleViolation
case class UnexpectedType(expectedType: String, errorType: String = "unexpectedType") extends ValidationRuleViolation
case class DateFormatMismatch(expectedFormat: String = "yyyy-MM-dd", errorType: String = "dateFormatMismatch") extends ValidationRuleViolation
case class EmptyString(errorType: String = "emptyString") extends ValidationRuleViolation
case class RegExMismatch(regex: String, errorType: String = "regularExpressionMismatch") extends ValidationRuleViolation
case class EnumValueMismatch(allowedValues: List[Any], errorType: String = "enumValueMismatch") extends ValidationRuleViolation
case class NotAnyOf(allowedAlternatives: List[(String, List[String])], errorType: String = "notAnyOf") extends ValidationRuleViolation
case class SmallerThanMinimumValue(minimumValue: Number, errorType: String = "smallerThanMinimumValue") extends ValidationRuleViolation
case class GreaterThanMaximumValue(maximumValue: Number, errorType: String = "greaterThanMaximumValue") extends ValidationRuleViolation
case class SmallerThanOrEqualToExclusiveMinimumValue(exclusiveMinimumValue: Number, errorType: String = "smallerThanOrEqualToExclusiveMinimumValue") extends ValidationRuleViolation
case class GreaterThanOrEqualToExclusiveMaximumValue(exclusiveMaximumValue: Number, errorType: String = "greaterThanOrEqualToExclusiveMaximumValue") extends ValidationRuleViolation
case class LessThanMinimumNumberOfItems(minimumItems: Int, errorType: String = "lessThanMinimumNumberOfItems") extends ValidationRuleViolation
case class MoreThanMaximumNumberOfItems(maximumItems: Int, errorType: String = "moreThanMaximumNumberOfItems") extends ValidationRuleViolation
case class OtherViolation(message: String, errorType: String = "otherViolation") extends ValidationRuleViolation
