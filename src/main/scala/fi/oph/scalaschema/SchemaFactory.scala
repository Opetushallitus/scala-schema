package fi.oph.scalaschema

import java.lang
import java.lang.reflect.Constructor

import fi.oph.scalaschema.Annotations.findAnnotations
import fi.oph.scalaschema.annotation._
import org.apache.commons.lang3.StringEscapeUtils
import org.reflections.Reflections

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}

object SchemaFactory {
  val defaultAnnotations: List[Class[_ <: Metadata]] = List(classOf[Description], classOf[MaxItems], classOf[MinItems], classOf[MaxValue], classOf[MinValue], classOf[RegularExpression])
  lazy val default = SchemaFactory(defaultAnnotations)
}

case class SchemaFactory(annotationsSupported: List[Class[_ <: Metadata]] = Nil) {
  def createSchema(className: String): SchemaWithClassName = {
    createSchema(typeByName(className), ScanState()).asInstanceOf[SchemaWithClassName]
  }

  def createSchema(clazz: Class[_]): SchemaWithClassName = {
    createSchema(clazz.getName)
  }

  private def typeByName(className: String): ru.Type = {
    val tyep: ru.Type = reflect.runtime.currentMirror.classSymbol(Class.forName(className)).toType
    tyep
  }

  private case class ScanState(root: Boolean = true, foundTypes: collection.mutable.Set[String] = collection.mutable.Set.empty, createdTypes: collection.mutable.Set[SchemaWithClassName] = collection.mutable.Set.empty) {
    def childState = copy(root = false)
  }

  private def createSchema(tpe: ru.Type, state: ScanState): Schema = {
    val typeName = tpe.typeSymbol.fullName

    if (typeName == "scala.Some") {
      createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, state)
    } else if (typeName == "scala.Option") {
      // Option[T] becomes the schema of T with required set to false
      OptionalSchema(createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, state))
    } else if (isListType(tpe)) {
      // (Traversable)[T] becomes a schema with items set to the schema of T
      ListSchema(createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, state))
    } else {
      schemaTypeForScala.getOrElse(typeName, {
        if (tpe.typeSymbol.isClass) {
          createClassOrTraitSchema(tpe, state)
        } else {
          throw new RuntimeException("Unsupported type: " + tpe)
        }
      })
    }
  }

  private lazy val schemaTypeForScala = Map(
    "org.joda.time.DateTime" -> DateSchema(),
    "java.util.Date" -> DateSchema(),
    "java.time.LocalDate" -> DateSchema(),
    "java.lang.String" -> StringSchema(),
    "scala.Boolean" -> BooleanSchema(),
    "scala.Int" -> NumberSchema(),
    "scala.Long" -> NumberSchema(),
    "scala.Double" -> NumberSchema(),
    "scala.Float" -> NumberSchema()
  )

  private def addToState(tyep: SchemaWithClassName, state: ScanState) = {
    state.createdTypes.add(tyep)
    tyep
  }

  private def createClassOrTraitSchema(tpe: ru.Type, state: ScanState) = {
    val className: String = tpe.typeSymbol.fullName
    if (!state.foundTypes.contains(className)) {
      state.foundTypes.add(className)

      val newSchema = if (tpe.typeSymbol.isAbstract) {
        AnyOfSchema(findImplementations(tpe, state), className)
      } else {
        createClassSchema(tpe, state)
      }

      if (state.root) {
        val classTypeDefinitions = state.createdTypes.toList
        newSchema.withDefinitions(definitions = classTypeDefinitions.sortBy(_.simpleName))
      } else {
        addToState(newSchema, state)
        createClassRefSchema(tpe)
      }

    } else {
      createClassRefSchema(tpe)
    }
  }

  private def createClassRefSchema(tpe: ru.Type) = applyMetadataAnnotations(tpe.typeSymbol, ClassRefSchema(tpe.typeSymbol.fullName, Nil))

  private def createClassSchema(tpe: ru.Type, state: ScanState) = {
    val traits: List[ru.Type] = findTraits(tpe)

    val className: String = tpe.typeSymbol.fullName
    state.foundTypes.add(className)

    val constructorParams: List[ru.Symbol] = tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption.getOrElse(Nil)
    val syntheticProperties: List[ru.Symbol] = (tpe.members ++ traits.flatMap(_.members)).filter(_.isMethod).filter (!findAnnotations(_, List(classOf[SyntheticProperty])).isEmpty)
      .map(sym => (sym.name, sym)).toMap.values.toList // <- deduplicate by term name
      .filterNot(sym => constructorParams.map(_.name).contains(sym.name)) // <- remove if overridden in case class constructor

    val propertySymbols = constructorParams ++ syntheticProperties

    val properties: List[Property] = propertySymbols.map { paramSymbol =>

      val term = paramSymbol.asTerm
      val termType = createSchema(term.typeSignature, state.childState)
      val termName: String = term.name.decoded.trim
      val ownerTrait = paramSymbol.owner.isAbstract match {
        case true =>
          Some(paramSymbol.owner)
        case false =>
          None
      }
      val property = applyMetadataAnnotations(term, Property(termName, termType, Nil))
      val matchingMethodsFromTraits = traits.flatMap (_.members
        .filter(_.isMethod)
        .filter(_.asTerm.asMethod.name.toString == termName )
        .filterNot(method => ownerTrait.contains(method.owner)) // deduplicate traits, in case this property is a trait method
      ).map(_.asTerm).distinct
      val propertyWithTraits = matchingMethodsFromTraits.foldLeft(property) { (property, traitMethod) =>
        applyMetadataAnnotations(traitMethod, property)
      }
      (paramSymbol.isMethod, propertyWithTraits.schema) match {
        case (_, s@OptionalSchema(itemSchema)) => propertyWithTraits
        case (true, schema) => propertyWithTraits.copy(schema = OptionalSchema(schema)) // synthetic properties are always optional
        case _ => propertyWithTraits
      }
    }.toList

    applyMetadataAnnotations(tpe.typeSymbol, ClassSchema(className, properties, Nil))
  }

  private def findTraits(tpe: ru.Type) = {
    tpe.baseClasses
      .map(_.fullName)
      .filter(!List("scala.Any").contains(_))
      .map {typeByName(_)}
      .filter {_.typeSymbol.asClass.isTrait}
  }

  private def applyMetadataAnnotations[T <: ObjectWithMetadata[T]](symbol: ru.Symbol, x: T): T = {
    findAnnotations(symbol, annotationsSupported).asInstanceOf[List[Metadata]].foldLeft(x) {
      case (current: T, metadata) => metadata.applyMetadata(current, this).asInstanceOf[T]
    }
  }

  private def isListType(tpe: ru.Type): Boolean = {
    tpe.baseClasses.exists(s => s.fullName == "scala.collection.Traversable" ||
      s.fullName == "scala.Array" ||
      s.fullName == "scala.Seq" ||
      s.fullName == "scala.List" ||
      s.fullName == "scala.Vector")
  }

  private def findImplementations(traitType: ru.Type, state: ScanState): List[SchemaWithClassName] = {
    val implementationClasses = TraitImplementationFinder.findTraitImplementations(traitType)

    import reflect.runtime.currentMirror
    implementationClasses.toList.map { klass =>
      createSchema(currentMirror.classSymbol(klass).toType, state).asInstanceOf[SchemaWithClassName]
    }
  }
}

private object TraitImplementationFinder {
  import collection.JavaConverters._
  val cache: collection.mutable.Map[String, List[Class[_]]] = collection.mutable.Map.empty
  val reflectionsCache: collection.mutable.Map[String, Reflections] = collection.mutable.Map.empty

  def findTraitImplementations(tpe: ru.Type): List[Class[_]] = this.synchronized {
    val className: String = tpe.typeSymbol.asClass.fullName

    cache.getOrElseUpdate(className, {
      val javaClass: Class[_] = Class.forName(className)
      val reflections = reflectionsCache.getOrElseUpdate(javaClass.getPackage.getName, new Reflections(javaClass.getPackage.getName))

      val implementationClasses = reflections.getSubTypesOf(javaClass).asScala.toSet.asInstanceOf[Set[Class[_]]].filter(!_.isInterface)
      implementationClasses.toList.sortBy(_.getName)
    })
  }
}

object Annotations {
  def findAnnotations(symbol: ru.Symbol, annotationsSupported: List[Class[_ <: StaticAnnotation]]): List[StaticAnnotation] = {
    symbol.annotations.flatMap { annotation =>
      val annotationType: String = annotation.tree.tpe.toString
      annotationsSupported.find(_.getName == annotationType) map { annotationClass =>
        val annotationParams: List[String] = annotation.tree.children.tail.map(str => StringEscapeUtils.unescapeJava(str.toString.replaceAll("\"$|^\"", "")))
        Annotations.parseAnnotation(annotationClass, annotationParams)
      }
    }
  }

  private def parseAnnotation(annotationClass: Class[_ <: StaticAnnotation], params: List[String]): StaticAnnotation = {
    val StringClass = classOf[String]
    val DoubleClass = classOf[Double]
    val IntegerClass = classOf[Int]

    val constructor: Constructor[_] = annotationClass.getConstructors()(0)
    val constructorParams: Array[Object] = constructor.getParameterTypes.zipWithIndex.map {
      case (StringClass, index) => params(index)
      case (DoubleClass, index) => new lang.Double(params(index).toDouble)
      case (IntegerClass, index) => new lang.Integer(params(index).toDouble.toInt)
      case (tyep, _) =>
        // Only a handful of types supported at the moment
        throw new IllegalArgumentException("Argument type not supported: " + tyep)
    }
    constructor.newInstance(constructorParams:_*).asInstanceOf[StaticAnnotation]
  }

}