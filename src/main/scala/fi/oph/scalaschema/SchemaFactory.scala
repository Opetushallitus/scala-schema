package fi.oph.scalaschema

import java.lang
import java.lang.reflect.Constructor
import java.sql.Timestamp
import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import fi.oph.scalaschema.Annotations.findAnnotations
import fi.oph.scalaschema.annotation._
import org.apache.commons.lang3.StringEscapeUtils
import org.joda.time.DateTime
import org.reflections.Reflections

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}
import scala.util.Try

object SchemaFactory {
  val defaultAnnotations: List[Class[_ <: Metadata]] = List(classOf[Title], classOf[Description], classOf[DefaultValue],
    classOf[MaxItems], classOf[MinItems], classOf[MaxValue], classOf[MinValue], classOf[MinValueExclusive], classOf[MaxValueExclusive],
    classOf[RegularExpression], classOf[OnlyWhen],
    classOf[EnumValue], classOf[Discriminator], classOf[IgnoreInAnyOfDeserialization])
  lazy val default = SchemaFactory(defaultAnnotations)
}

case class SchemaFactory(annotationsSupported: List[Class[_ <: Metadata]] = Nil) {
  private val cachedSchemas: collection.mutable.Map[ru.Type, Schema] = collection.mutable.Map.empty

  def createSchema(className: String): SchemaWithClassName = {
    getCachedSchema(typeByName(className)).asInstanceOf[SchemaWithClassName]
  }

  def createSchema(clazz: Class[_]): SchemaWithClassName = {
    createSchema(clazz.getName)
  }

  def createSchema(tpe: ru.Type): Schema = {
    getCachedSchema(tpe)
  }

  def createSchema[T](implicit tag: ru.TypeTag[T]): Schema = {
    getCachedSchema(tag.tpe)
  }

  private def getCachedSchema(tpe: ru.Type) = synchronized {
    cachedSchemas.getOrElseUpdate(tpe, createSchema(tpe, ScanState()))
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
      createSchema(typeArgs(tpe).head, state)
    } else if (typeName == "scala.Option") {
      // Option[T] becomes the schema of T with required set to false
      OptionalSchema(createSchema(typeArgs(tpe).head, state))
    } else if (isMapType(tpe)) {
      if(typeArgs(tpe).head.typeSymbol.fullName != "java.lang.String") {
        throw new IllegalArgumentException("Maps are only supported with String keys")
      }
      MapSchema(createSchema(typeArgs(tpe)(1), state))
    } else if (isListType(tpe)) {
      // (Traversable)[T] becomes a schema with items set to the schema of T
      ListSchema(createSchema(typeArgs(tpe).head, state))
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

  private def typeArgs(tpe: ru.Type): List[ru.Type] = tpe match {
    case t: ru.TypeRefApi => t.args
    case t: ru.NullaryMethodTypeApi => typeArgs(t.resultType)
  }

  private lazy val schemaTypeForScala = Map(
    "org.joda.time.DateTime" -> DateSchema(dateType = classOf[DateTime]),
    "java.util.Date" -> DateSchema(dateType = classOf[Date]),
    "java.sql.Timestamp" -> DateSchema(dateType = classOf[Timestamp]),
    "java.time.LocalDate" -> DateSchema(dateType = classOf[LocalDate]),
    "java.time.ZonedDateTime" -> DateSchema(dateType = classOf[ZonedDateTime]),
    "java.lang.String" -> StringSchema(),
    "scala.Boolean" -> BooleanSchema(),
    "scala.Int" -> NumberSchema(numberType = classOf[Int]),
    "scala.Long" -> NumberSchema(numberType = classOf[Long]),
    "scala.Double" -> NumberSchema(numberType = classOf[Double]),
    "scala.Float" -> NumberSchema(numberType = classOf[Float]),
    "java.lang.Integer" -> NumberSchema(numberType = classOf[Integer]),
    "java.lang.Float" -> NumberSchema(numberType = classOf[java.lang.Float]),
    "java.lang.Long" -> NumberSchema(numberType = classOf[java.lang.Long]),
    "java.lang.Double" -> NumberSchema(numberType = classOf[java.lang.Double]),
    "java.math.BigDecimal" -> NumberSchema(numberType = classOf[java.math.BigDecimal]),
    classOf[BigDecimal].getName -> NumberSchema(numberType = classOf[BigDecimal]),
    classOf[BigInt].getName -> NumberSchema(numberType = classOf[BigInt]),
    "org.json4s.JsonAST.JValue" -> AnySchema(),
    "org.json4s.JsonAST.JObject" -> AnyObjectSchema(),
    "org.json4s.JsonAST.JArray" -> AnyListSchema()
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
        applyMetadataFromClassAndTraits(tpe, AnyOfSchema(findImplementations(tpe, state.childState), className, Nil))
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

  private def createClassRefSchema(tpe: ru.Type) = applyMetadataFromClassAndTraits(tpe, ClassRefSchema(tpe.typeSymbol.fullName, Nil))

  private def createClassSchema(tpe: ru.Type, state: ScanState) = {
    import MemberFinder.members
    val traits: List[ru.Type] = findTraits(tpe)

    val className: String = tpe.typeSymbol.fullName

    state.foundTypes.add(className)

    val constructorParams: List[(ru.Symbol, Boolean)] = tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption.getOrElse(Nil).map((_, false))

    val syntheticProperties: List[(ru.Symbol, Boolean)] = (members(tpe) ++ traits.flatMap(members)).filter(_.isMethod).filter (!findAnnotations(_, List(classOf[SyntheticProperty])).isEmpty)
      .map(sym => (sym.name, sym)).toMap.values.toList // <- deduplicate by term name
      .filterNot(sym => constructorParams.map(_._1.name).contains(sym.name)) // <- remove if overridden in case class constructor
      .map((_, true))

    val propertySymbols = constructorParams ++ syntheticProperties

    val properties: List[Property] = propertySymbols.map { case (paramSymbol, synthetic) =>
      val term = paramSymbol.asTerm
      val termSchema = createSchema(term.typeSignature, state.childState)
      val termName: String = term.name.decoded.trim
      val ownerTrait = paramSymbol.owner.isAbstract match {
        case true =>
          Some(paramSymbol.owner)
        case false =>
          None
      }
      val property = applyMetadataAnnotations(term, Property(termName, termSchema, Nil, synthetic))
      val matchingMethodsFromTraits = traits.flatMap (t => members(t)
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
    }

    applyMetadataFromClassAndTraits(tpe, ClassSchema(className, properties, Nil))
  }

  private def findTraits(tpe: ru.Type) = {
    tpe.baseClasses
      .map(_.fullName)
      .filter(!List("scala.Any").contains(_))
      .map {typeByName(_)}
      .filter {_.typeSymbol.asClass.isTrait}
      .filterNot {_ == tpe}
  }

  private def applyMetadataFromClassAndTraits[T <: ObjectWithMetadata[T]](tpe: ru.Type, schema: T): T =
    applyMetadataAnnotations[T](tpe.typeSymbol, findTraits(tpe).foldLeft(schema) { (schema, t) =>
      applyMetadataAnnotations[T](t.typeSymbol, schema)
    })

  private def applyMetadataAnnotations[T <: ObjectWithMetadata[T]](symbol: ru.Symbol, x: T): T = {
    findAnnotations(symbol, annotationsSupported).asInstanceOf[List[Metadata]].foldLeft(x) {
      case (current, metadata) => metadata.applyMetadata(current, this).asInstanceOf[T]
    }
  }

  private def isListType(tpe: ru.Type): Boolean = {
    tpe.baseClasses.exists(s => s.fullName == "scala.collection.Traversable" ||
      s.fullName == "scala.Array" ||
      s.fullName == "scala.Seq" ||
      s.fullName == "scala.List" ||
      s.fullName == "scala.Vector")
  }

  private def isMapType(tpe: ru.Type): Boolean = {
    tpe.baseClasses.exists(s => s.fullName == "scala.collection.immutable.Map")
  }

  private def findImplementations(traitType: ru.Type, state: ScanState): List[SchemaWithClassName] = {
    val implementationClasses = TraitImplementationFinder.findTraitImplementations(traitType)

    import reflect.runtime.currentMirror
    implementationClasses.toList.map { klass =>
      createSchema(currentMirror.classSymbol(klass).toType, state).asInstanceOf[SchemaWithClassName]
    }
  }
}

private object MemberFinder {
  val cache: collection.mutable.Map[String, List[ru.Symbol]] = collection.mutable.Map.empty

  def members(tpe: ru.Type): List[ru.Symbol] = this.synchronized {
    val className: String = tpe.typeSymbol.asClass.fullName
    cache.getOrElseUpdate(className, {
      tpe.decls.sorted
    })
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
      val packageName = javaClass.getPackage.getName
      if (packageName.startsWith("java.")) {
        throw new RuntimeException("Cannot use java.* interfaces as traits in Schemas")
      }
      val reflections = reflectionsCache.getOrElseUpdate(packageName, new Reflections(packageName))

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
        val annotationParams: List[ru.Tree] = annotation.tree.children.tail
        Annotations.parseAnnotation(annotationClass, annotationParams)
      }
    }
  }

  import scala.tools.reflect.ToolBox
  private val tb = reflect.runtime.currentMirror.mkToolBox()

  private def unescapeJava(str: Any) = StringEscapeUtils.unescapeJava(str.toString.replaceAll("\"$|^\"", ""))

  private def parseAnnotation(annotationClass: Class[_ <: StaticAnnotation], params: List[ru.Tree]): StaticAnnotation = {
    val StringClass = classOf[String]
    val DoubleClass = classOf[Double]
    val IntegerClass = classOf[Int]
    val BooleanClass = classOf[Boolean]

    val constructor: Constructor[_] = annotationClass.getConstructors()(0)

    def parseAsDouble(v: Any) = new lang.Double(v.toString.toDouble)
    def parseAsInteger(v: Any) = new lang.Integer(v.toString.toDouble.toInt)
    def parseAsBoolean(v: Any) = new lang.Boolean(v.toString.toBoolean)

    def parseAnnotationParam(klass: Class[_], value: ru.Tree): AnyRef = (klass, value) match {
      case (_, value) if (value.toString.startsWith("\"")) => unescapeJava(value)
      case (_, value) if (value.toString == "scala.None") => None
      case (DoubleClass, value) => parseAsDouble(value)
      case (IntegerClass, value) => parseAsInteger(value)
      case (BooleanClass, value) => parseAsBoolean(value)
      case (tyep, value) =>
        Try(parseAsInteger(value.toString.toInt))
          .orElse(Try(parseAsDouble(value.toString.toDouble)))
          .orElse(Try(parseAsBoolean(value.toString.toBoolean)))
          .getOrElse {
            val evaluated = tb.eval(tb.untypecheck(value))
            //println("Expensive: " + annotationClass.getName + " / " + tyep.getName + " = " + value)
            evaluated.asInstanceOf[AnyRef]
          }
    }

    val constructorParams: Array[Object] = constructor.getParameterTypes.zipWithIndex
      .map {
        case (klass, index) => parseAnnotationParam(klass, params(index))
      }

    try {
      constructor.newInstance(constructorParams:_*).asInstanceOf[StaticAnnotation]
    } catch {
      case e: IllegalArgumentException =>
        throw new RuntimeException(s"Error parsing annotation $annotationClass with params $params resulting to constructorParams ${constructorParams.toList.map(_.getClass.getName)} while expecting ${constructor.getParameterTypes.toList.map(_.getName)}", e)
    }
  }
}