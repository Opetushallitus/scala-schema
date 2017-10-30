package fi.oph.scalaschema.extraction

import fi.oph.scalaschema.annotation.{MaxValue, MaxValueExclusive, MinValue, MinValueExclusive}
import fi.oph.scalaschema._
import org.json4s.JsonAST.{JDecimal, JDouble, JInt, JLong}
import org.json4s._

object NumberExtractor extends ExtractorWithDefaultValueSupport[Number, NumberSchema] {
  def extractExisting(cursor: JsonCursor, ns: NumberSchema, metadata: List[Metadata])(implicit context: ExtractionContext): Either[List[ValidationError], Number] = {
    val extractionResult: Either[List[ValidationError], Number] = cursor.json match {
      // NOTE: Number types don't necessarily match correctly and because of type erasure, an Option[Int] may end up containing a Double.
      case JDouble(num: Double) => Right(num)
      case JInt(num: BigInt) => Right(num)
      case JDecimal(num: BigDecimal) => Right(num)
      case JLong(num) => Right(num.toLong)
      case JString(num) =>  try {
          Right(parseNumber(num, ns.numberType))
        } catch {
          case e: NumberFormatException => Left(List(ValidationError(cursor.path, cursor.json, UnexpectedType("number"))))
        }
      case json =>
        Left(List(ValidationError(cursor.path, json, UnexpectedType("number"))))
    }
    extractionResult.right.map(num => convertNumber(num, ns.numberType)).flatMap { number: Number =>
      context.ifValidating(((metadata ++ ns.metadata).collect {
        case MinValue(minValue) if number.doubleValue < minValue => ValidationError(cursor.path, cursor.json, SmallerThanMinimumValue(minValue))
        case MaxValue(maxValue) if number.doubleValue > maxValue => ValidationError(cursor.path, cursor.json, GreaterThanMaximumValue(maxValue))
        case MinValueExclusive(minValue) if number.doubleValue <= minValue => ValidationError(cursor.path, cursor.json, SmallerThanOrEqualToExclusiveMinimumValue(minValue))
        case MaxValueExclusive(maxValue) if number.doubleValue >= maxValue => ValidationError(cursor.path, cursor.json, GreaterThanOrEqualToExclusiveMaximumValue(maxValue))
      })) ++ {
        EnumValues.verifyEnumValue[Number](cursor.path, ns.enumValues, number, Serializer.serializeNumber).left.getOrElse(Nil)
      } match {
        case Nil => Right(number)
        case errors => Left(errors)
      }
    }
  }

  private def parseNumber(str: String, klass: Class[_]): Number = {
    if (klass == classOf[Int] || klass == classOf[Integer]) {
      str.toInt
    } else if (klass == classOf[Float]) {
      str.toFloat
    } else if (klass == classOf[Double]) {
      str.toDouble
    } else if (klass == classOf[Int]) {
      str.toInt
    } else if (klass == classOf[Long]) {
      str.toLong
    } else if (klass == classOf[BigInt]) {
      BigInt(str)
    } else if (klass == classOf[BigDecimal]) {
      BigDecimal(str)
    } else if (klass == classOf[Integer]) {
      Integer.valueOf(str)
    } else if (klass == classOf[java.lang.Float]) {
      java.lang.Float.valueOf(str)
    } else if (klass == classOf[java.lang.Long]) {
      java.lang.Long.valueOf(str)
    } else if (klass == classOf[java.lang.Double]) {
      java.lang.Double.valueOf(str)
    } else if (klass == classOf[java.math.BigDecimal]) {
      new java.math.BigDecimal(str)
    } else {
      throw new UnsupportedOperationException("Unrecognized Number type: " + klass.getName)
    }
  }

  private def convertNumber(number: Number, klass: Class[_]): Number =  {
    if (klass == classOf[Int] || klass == classOf[Integer]) {
      number.intValue
    } else if (klass == classOf[Float] || klass == classOf[java.lang.Float]) {
      number.floatValue
    } else if (klass == classOf[Double] || klass == classOf[java.lang.Double]) {
      number.doubleValue
    } else if (klass == classOf[Long] || klass == classOf[java.lang.Long]) {
      number.longValue
    } else if (klass == classOf[BigInt]) {
      if (!number.isInstanceOf[BigInt]) {
        BigInt(number.longValue)
      } else {
        number
      }
    } else if (klass == classOf[java.math.BigDecimal]) {
      if (!number.isInstanceOf[java.math.BigDecimal]) {
        new java.math.BigDecimal(number.doubleValue)
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
