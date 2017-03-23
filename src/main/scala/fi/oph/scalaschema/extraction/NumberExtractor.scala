package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{MaxValue, MaxValueExclusive, MinValue, MinValueExclusive}
import fi.oph.scalaschema.{ExtractionContext, Metadata, NumberSchema}
import org.json4s.JsonAST.{JDecimal, JDouble, JInt, JLong}
import org.json4s._

object NumberExtractor {
  def extractNumber(json: JValue, schema: NumberSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Number] = {
    val extractionResult: Either[List[ValidationError], Number] = json match {
      // NOTE: Number types don't necessarily match correctly and because of type erasure, an Option[Int] may end up containing a Double.
      case JDouble(num: Double) => Right(num)
      case JInt(num: BigInt) => Right(num.intValue)
      case JDecimal(num: BigDecimal) => Right(num.doubleValue)
      case JLong(num) => Right(num.toLong)
      case _ => Left(List(ValidationError(context.path, json, UnexpectedType("number"))))
    }
    extractionResult.right.flatMap { number: Number =>
      context.ifValidating(((metadata ++ schema.metadata).collect {
        case MinValue(minValue) if number.doubleValue < minValue => ValidationError(context.path, json, SmallerThanMinimumValue(minValue))
        case MaxValue(maxValue) if number.doubleValue > maxValue => ValidationError(context.path, json, GreaterThanMaximumValue(maxValue))
        case MinValueExclusive(minValue) if number.doubleValue <= minValue => ValidationError(context.path, json, SmallerThanOrEqualToExclusiveMinimumValue(minValue))
        case MaxValueExclusive(maxValue) if number.doubleValue >= maxValue => ValidationError(context.path, json, GreaterThanOrEqualToExclusiveMaximumValue(maxValue))
      })) ++ {
        EnumValues.verifyEnumValue(schema.enumValues, number).left.getOrElse(Nil)
      } match {
        case Nil => Right(number)
        case errors => Left(errors)
      }
    }
  }
}
