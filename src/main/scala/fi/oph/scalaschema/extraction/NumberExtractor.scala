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
      case JInt(num: BigInt) => Right(num)
      case JDecimal(num: BigDecimal) => Right(num)
      case JLong(num) => Right(num.toLong)
      case _ =>
        Left(List(ValidationError(context.path, json, UnexpectedType("number"))))
    }
    extractionResult.right.map(num => convertNumber(num, schema.numberType)).flatMap { number: Number =>
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


  private def convertNumber(number: Number, klass: Class[_]): Number =  {
    if (klass == classOf[Int] || klass == classOf[Integer]) {
      number.intValue
    } else if (klass == classOf[Float]) {
      number.floatValue
    } else if (klass == classOf[Double]) {
      number.doubleValue
    } else if (klass == classOf[Int]) {
      number.intValue
    } else if (klass == classOf[Long]) {
      number.longValue
    } else if (klass == classOf[BigInt]) {
      if (!number.isInstanceOf[BigInt]) {
        BigInt(number.longValue)
      } else {
        number
      }
    } else if (klass == classOf[BigDecimal]) {
      if (!number.isInstanceOf[BigDecimal]) {
        BigDecimal(number.doubleValue)
      } else {
        number
      }
    } else {
      throw new UnsupportedOperationException("Unrecognized Number type: " + klass.getName)
    }
  }
}
