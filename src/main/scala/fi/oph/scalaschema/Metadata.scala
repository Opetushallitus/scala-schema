package fi.oph.scalaschema

import java.lang
import java.lang.reflect.Constructor

import org.json4s.JsonAST
import org.json4s.JsonAST.{JNothing, JObject, JString}

import scala.annotation.StaticAnnotation

trait Metadata extends StaticAnnotation

trait ObjectWithMetadata[T <: ObjectWithMetadata[T]] {
  def metadata: List[Metadata]
  def replaceMetadata(newMetadata: List[Metadata]): ObjectWithMetadata[T]
  def appendMetadata(newMetadata: List[Metadata]): ObjectWithMetadata[T] = replaceMetadata(metadata ++ newMetadata)
}

trait MetadataSupport[M] extends AnnotationSupport[M] with JsonMetadataSupport[M] // <- make M Metadata

trait AnnotationSupport[M] {
  def applyMetadata(x: ObjectWithMetadata[_], metadata: M, schemaFactory: SchemaFactory) = x.appendMetadata(List(metadata.asInstanceOf[Metadata]))

  final def applyAnnotations(annotationClass: String, params: List[String], x: ObjectWithMetadata[_], schemaFactory: SchemaFactory) = if (annotationClass == metadataClass.getName) {
    applyMetadata(x, parseAnnotation(params), schemaFactory)
  } else {
    x
  }

  def metadataClass: Class[M]

  def parseAnnotation(params: List[String]): M = {
    val StringClass = classOf[String]
    val DoubleClass = classOf[Double]
    val IntegerClass = classOf[Int]

    val constructor: Constructor[_] = metadataClass.getConstructors()(0)
    val constructorParams: Array[Object] = constructor.getParameterTypes.zipWithIndex.map {
      case (StringClass, index) => params(index)
      case (DoubleClass, index) => new lang.Double(params(index).toDouble)
      case (IntegerClass, index) => new lang.Integer(params(index).toDouble.toInt)
      case (tyep, _) =>
        // Only a handful of types supported at the moment
        throw new IllegalArgumentException("Argument type not supported: " + tyep)
    }
    constructor.newInstance(constructorParams:_*).asInstanceOf[M]
  }
}

trait JsonMetadataSupport[M] {
  def appendMetadataToJsonSchema(obj: JObject, metadata: M): JObject

  def appendMetadataToJsonSchema(obj: JObject, metadata: Metadata): JObject = if (metadataClass.isInstance(metadata)) {
    appendMetadataToJsonSchema(obj, metadata.asInstanceOf[M])
  } else {
    obj
  }
  def appendToDescription(obj: JObject, newDescription: String): JsonAST.JObject = {
    val description = obj.\("description") match {
      case JString(s) => s + ".\n" + newDescription
      case JNothing => newDescription
    }
    obj.merge(JObject("description" -> JString(description)))
  }

  def addEnumValue(value: String, p: Property): Property = {
    val newSchema = p.schema match {
      case StringSchema(enumValues) =>
        StringSchema(Some(enumValues.toList.flatten ++ List(value)))
      case x: Any => throw new RuntimeException("Unexpected schema: " + x)

    }
    p.copy(schema = newSchema)
  }

  def metadataClass: Class[M]
}
