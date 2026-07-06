package fi.oph.scalaschema

import java.lang.reflect.Constructor
import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime, ZonedDateTime, OffsetDateTime}
import java.util.Date

import fi.oph.scalaschema.Annotations.findAnnotations
import fi.oph.scalaschema.annotation._
import org.apache.commons.text.StringEscapeUtils
import org.joda.time.DateTime
import org.reflections.Reflections

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}
import scala.util.Try

object SchemaFactory {
  lazy val default = SchemaFactory()
}

case class SchemaFactory() {
  private val cachedSchemas: collection.mutable.Map[ru.Type, Schema] = collection.mutable.Map.empty

  def createSchema(className: String): SchemaWithClassName = synchronized {
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

  def createSchema(classRef: ClassRefSchema, rootSchema: Schema): SchemaWithClassName = {
    rootSchema match {
      case s: SchemaWithDefinitions =>
        s.findSchemaForClassRef(classRef).getOrElse(createSchemaWithoutRootSchema(classRef))
      case _ =>
        createSchemaWithoutRootSchema(classRef)
    }
  }

  private[scalaschema] def createSchemaWithoutRootSchema(classRef: ClassRefSchema): SchemaWithClassName =
    classRef.definitionKey.variantQualifier match {
      case Some(_) =>
        throw new IllegalArgumentException(
          s"Path-specific schema ref ${classRef.definitionName} for ${classRef.fullClassName} was not found in root schema definitions"
        )
      case None =>
        createSchema(classRef.fullClassName)
    }

  private def getCachedSchema(tpe: ru.Type) = synchronized {
    cachedSchemas.getOrElseUpdate(tpe, createSchema(tpe, ScanState.fromRootSchemaType(tpe)))
  }

  private val typeByNameCache: collection.mutable.Map[String, ru.Type] = collection.mutable.Map.empty

  private def typeByName(className: String): ru.Type = {
    typeByNameCache.getOrElseUpdate(className, {
      reflect.runtime.currentMirror.classSymbol(Class.forName(className)).toType
    })
  }

  private case class IncludedComputedProperty(ownerClassName: String, propertySuffixPath: List[String]) {
    def matchesProperty(ownerClassNames: List[String], candidatePath: List[String]): Boolean =
      ownerClassNames.contains(this.ownerClassName) &&
        candidatePath.endsWith(propertySuffixPath)
  }

  private object IncludedComputedProperty {
    def fromAnnotation(annotation: IncludeComputedProperty): IncludedComputedProperty =
      IncludedComputedProperty(annotation.owner.getName, parsePropertySuffixPath(annotation.propertySuffixPath))

    private def parsePropertySuffixPath(propertySuffixPath: String): List[String] =
      propertySuffixPath.split("\\.").map(_.trim).filter(_.nonEmpty).toList match {
        case Nil => throw new RuntimeException("@IncludeComputedProperty propertySuffixPath must not be empty")
        case pathSegments => pathSegments
      }
  }

  private case class ScanState(
    root: Boolean,
    registeredDefinitionRefs: collection.mutable.Set[DefinitionKey],
    createdDefinitions: collection.mutable.Set[SchemaWithClassName],
    includedComputedProperties: Set[IncludedComputedProperty],
    path: List[String]
  ) {
    def childState(propertyName: Option[String] = None): ScanState =
      copy(root = false, path = propertyName.map(path :+ _).getOrElse(path))

    def registerDefinitionRef(definitionKey: DefinitionKey): Boolean =
      registeredDefinitionRefs.add(definitionKey)

    def addCreatedDefinition(schema: SchemaWithClassName): Unit =
      createdDefinitions.add(schema)

    def isComputedPropertyIncluded(ownerClassNames: List[String], propertyName: String): Boolean =
      includedComputedProperties.exists(_.matchesProperty(ownerClassNames, path :+ propertyName))
  }

  private object ScanState {
    def fromRootSchemaType(tpe: ru.Type): ScanState =
      ScanState(
        root = true,
        registeredDefinitionRefs = collection.mutable.Set.empty,
        createdDefinitions = collection.mutable.Set.empty,
        includedComputedProperties = readIncludedComputedPropertiesFromSchemaType(tpe),
        path = Nil
      )
  }

  private def findAnnotationsOfType[A <: StaticAnnotation](symbol: ru.Symbol)(implicit tag: ru.TypeTag[A]): List[A] =
    findAnnotations(symbol, { annotationSymbol => annotationSymbol == ru.typeOf[A].typeSymbol })
      .map(_.asInstanceOf[A])

  private def hasAnnotation[A <: StaticAnnotation](symbol: ru.Symbol)(implicit tag: ru.TypeTag[A]): Boolean =
    findAnnotationsOfType[A](symbol).nonEmpty

  private def readIncludedComputedPropertiesFromSchemaType(tpe: ru.Type): Set[IncludedComputedProperty] =
    findAnnotationsOfType[IncludeComputedProperty](tpe.typeSymbol)
      .map(IncludedComputedProperty.fromAnnotation)
      .toSet

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
      // (Iterable)[T] becomes a schema with items set to the schema of T
      ListSchema(createSchema(typeArgs(tpe).head, state))
    } else {
      schemaTypeForScala.getOrElse(typeName, {
        if (tpe.typeSymbol.isClass) {
          findFlattenAnnotation(tpe.typeSymbol) match {
            case None =>
              createClassOrTraitSchema(tpe, state, false)
            case Some(f: ReadFlattened) =>
              createClassOrTraitSchema(tpe, state, true)
            case Some(f: Flatten) =>
              createFlattenedSchema(tpe, state)
            case _ => throw new RuntimeException("Unsupported type: " + tpe)
          }
        } else {
          throw new RuntimeException("Unsupported type: " + tpe)
        }
      })
    }
  }

  private def findFlattenAnnotation(symbol: ru.Symbol): Option[StaticAnnotation] = {
    val checkIfFlatten = { symbol: ru.Symbol =>
      symbol.fullName == classOf[Flatten].getName || symbol.fullName == classOf[ReadFlattened].getName
    }
    findAnnotations(symbol, checkIfFlatten) match {
      case List(flatten) => Some(flatten)
      case Nil => None
      case _ => throw new RuntimeException(s"Multiple @Flatten or @ReadFlattened annotations found for $symbol")
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
    "java.time.LocalDateTime" -> DateSchema(dateType = classOf[LocalDateTime]),
    "java.time.ZonedDateTime" -> DateSchema(dateType = classOf[ZonedDateTime]),
    "java.time.OffsetDateTime" -> DateSchema(dateType = classOf[OffsetDateTime]),
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
    "org.json4s.JValue" -> AnySchema(),
    "org.json4s.JObject" -> AnyObjectSchema(),
    "org.json4s.JArray" -> AnyListSchema()
  )

  private def createClassOrTraitSchema(tpe: ru.Type, state: ScanState, readFlattened: Boolean) = {
    val definitionKey = definitionKeyFor(tpe, state)
    if (state.registerDefinitionRef(definitionKey)) {
      createNewClassOrTraitSchema(tpe, state, readFlattened, definitionKey)
    } else {
      createClassRefSchema(tpe, definitionKey)
    }
  }

  private def createNewClassOrTraitSchema(
    tpe: ru.Type,
    state: ScanState,
    readFlattened: Boolean,
    definitionKey: DefinitionKey
  ) = {
    val newSchema = createClassOrTraitSchemaBody(tpe, state, readFlattened, definitionKey)

    if (state.root) {
      val definitions = state.createdDefinitions.toList
      newSchema.withDefinitions(definitions = definitions.sortBy(_.definitionName))
    } else {
      state.addCreatedDefinition(newSchema)
      createClassRefSchema(tpe, definitionKey)
    }
  }

  private def createClassOrTraitSchemaBody(
    tpe: ru.Type,
    state: ScanState,
    readFlattened: Boolean,
    definitionKey: DefinitionKey
  ): SchemaWithDefinitions = {
    if (tpe.typeSymbol.isAbstract) {
      if (readFlattened) throw new RuntimeException(s"@ReadFlattened annotation on abstract class or trait $tpe")
      applyMetadataFromClassAndTraits(tpe, AnyOfSchema(definitionKey, findImplementations(tpe, state.childState())))
    } else {
      createClassSchema(tpe, state, readFlattened, definitionKey)
    }
  }

  private def createFlattenedSchema(tpe: ru.Type, state: ScanState) = {
    if (tpe.typeSymbol.isAbstract) throw new RuntimeException(s"@Flatten annotation on abstract class or trait $tpe")

    val definitionKey = definitionKeyFor(tpe, state)
    state.registerDefinitionRef(definitionKey)
    val classSchema = createClassSchema(tpe, state, false, definitionKey)

    classSchema.properties match {
      case List(property) => FlattenedSchema(classSchema, property)
      case Nil => throw new RuntimeException(s"@Flatten annotation on a case class with zero fields: $tpe")
      case _ => throw new RuntimeException(s"@Flatten annotation on a case class with more than one field: $tpe")
    }
  }

  private def createClassRefSchema(tpe: ru.Type, definitionKey: DefinitionKey) =
    applyMetadataFromClassAndTraits(tpe, ClassRefSchema(definitionKey))

  private def createClassSchema(
    tpe: ru.Type,
    state: ScanState,
    readFlattened: Boolean,
    definitionKey: DefinitionKey
  ): ClassSchema = {
    import MemberFinder.members
    val traits: List[ru.Type] = findTraits(tpe)

    val constructorParams: List[(ru.Symbol, Boolean)] = tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption.getOrElse(Nil).map((_, false))
    val syntheticProperties: List[(ru.Symbol, Boolean)] = (members(tpe) ++ traits.flatMap(members))
      .filter(_.isMethod)
      .filter { symbol =>
        val propertyIsComputed = hasAnnotation[ComputedProperty](symbol)
        val propertyIsSynthetic = hasAnnotation[SyntheticProperty](symbol)
        val computedPropertyOwnerClassNames =
          if (propertyIsComputed) getBaseClasses(typeByName(symbol.owner.fullName)) else Nil
        val computedPropertyIsIncluded =
          propertyIsComputed && state.isComputedPropertyIncluded(computedPropertyOwnerClassNames, propertyName(symbol))

        (!propertyIsComputed && propertyIsSynthetic) || (propertyIsComputed && computedPropertyIsIncluded)
      }
      .map(sym => (sym.name, sym)).toMap.values.toList // <- deduplicate by term name
      .filterNot(sym => constructorParams.map(_._1.name).contains(sym.name)) // <- remove if overridden in case class constructor
      .map((_, true))

    val propertySymbols = constructorParams ++ syntheticProperties

    val properties: List[Property] = propertySymbols.map { case (paramSymbol, synthetic) =>
      val term = paramSymbol.asTerm
      val termName: String = propertyName(term)
      val termSchema = createSchema(term.typeSignature, state.childState(Some(termName)))
      val computed = hasAnnotation[ComputedProperty](paramSymbol)
      val ownerTrait = paramSymbol.owner.isAbstract match {
        case true =>
          Some(paramSymbol.owner)
        case false =>
          None
      }
      val property = applyMetadataAnnotations(term, Property(termName, termSchema, Nil, synthetic, computed))
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

    val classSchema = applyMetadataFromClassAndTraits(tpe, ClassSchema(definitionKey, properties))

    if (readFlattened) {
      val requiredProperties = classSchema.properties.filter(!_.schema.isInstanceOf[OptionalSchema])
      requiredProperties match {
        case List(property) =>
          val flattenedSchema = FlattenedSchema(classSchema, property)
          classSchema.copy(readFlattened = Some(flattenedSchema))
        case Nil => throw new RuntimeException(s"@ReadFlattened annotation on a case class with zero required fields: $tpe")
        case _ => throw new RuntimeException(s"@ReadFlattened annotation on a case class with more than one required field: $tpe")
      }
    } else {
      classSchema
    }
  }

  private def definitionKeyFor(tpe: ru.Type, state: ScanState): DefinitionKey =
    DefinitionKey(tpe.typeSymbol.fullName, resolveSchemaVariantQualifierFor(tpe, state))

  private def resolveSchemaVariantQualifierFor(tpe: ru.Type, state: ScanState): Option[List[String]] = {
    val matchingQualifiers = state.includedComputedProperties.toList.flatMap { includedProperty =>
      val isPathSpecificComputedProperty = includedProperty.propertySuffixPath.length > 1

      if (isPathSpecificComputedProperty) {
        val schemaPath = includedProperty.propertySuffixPath.dropRight(1)
        val matchingPrefix = schemaPath.inits.toList
          .filter(_.nonEmpty)
          .find(prefix => state.path.endsWith(prefix))

        matchingPrefix.filter { prefix =>
          // Intermediate schemas are matched by path only, because the computed-property
          // owner type is not available until the full schema path is reached. This can
          // create unnecessary variants for unrelated suffix matches; use a more
          // qualified path from the root when that matters.
          val isFullSchemaPath = prefix == schemaPath
          val isIntermediateSchemaPath = !isFullSchemaPath
          val currentTypeCanExposeIncludedProperty =
            getBaseClasses(tpe).contains(includedProperty.ownerClassName)

          isIntermediateSchemaPath || currentTypeCanExposeIncludedProperty
        }
      } else {
        None
      }
    }

    // Use the most specific matching path qualifier.
    matchingQualifiers
      .sortBy(qualifier => (-qualifier.length, qualifier.mkString(".")))
      .headOption
  }

  private def propertyName(symbol: ru.Symbol): String =
    symbol.name.decodedName.toString.trim

  private def findTraits(tpe: ru.Type) = {
    tpe.baseClasses
      .map(_.fullName)
      .filter(!List("scala.Any").contains(_))
      .map(typeByName)
      .filter {_.typeSymbol.asClass.isTrait}
      .filterNot {_ == tpe}
  }

  private def applyMetadataFromClassAndTraits[T <: ObjectWithMetadata[T]](tpe: ru.Type, schema: T): T =
    applyMetadataAnnotations[T](tpe.typeSymbol, findTraits(tpe).foldLeft(schema) { (schema, t) =>
      applyMetadataAnnotations[T](t.typeSymbol, schema)
    })

  private def applyMetadataAnnotations[T <: ObjectWithMetadata[T]](symbol: ru.Symbol, x: T): T = {
    findMetadataAnnotations(symbol).foldLeft(x) {
      case (current, metadata) => metadata.applyMetadata(current, this).asInstanceOf[T]
    }
  }

  private def findMetadataAnnotations(symbol: ru.Symbol) = {
    val checkIfMetadataAnnotation = { symbol: ru.Symbol => symbol.asClass.baseClasses.contains(ru.typeOf[Metadata].typeSymbol) }
    findAnnotations(symbol, checkIfMetadataAnnotation).asInstanceOf[List[Metadata]]
  }

  private val baseClassCache: collection.mutable.Map[ru.Type, List[String]] = collection.mutable.Map.empty

  private def getBaseClasses(tpe: ru.Type): List[String] = {
    baseClassCache.getOrElseUpdate(tpe, {
      tpe.baseClasses.map(_.fullName)
    })
  }

  private def isListType(tpe: ru.Type): Boolean = {
    getBaseClasses(tpe).exists(s =>
      s == "scala.collection.Iterable" ||
      s == "scala.collection.IterableOnce" ||
      s == "scala.collection.IterableOnceOps" ||
      s == "scala.collection.IterableOps" ||
      s == "scala.collection.immutable.Iterable" ||
      s == "scala.collection.mutable.Iterable" ||
      s == "scala.Array" ||
      s == "scala.Seq" ||
      s == "scala.List" ||
      s == "scala.Vector")
  }

  private def isMapType(tpe: ru.Type): Boolean = {
    getBaseClasses(tpe).contains("scala.collection.immutable.Map")
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
  import scala.jdk.CollectionConverters._
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
  private val annotationCache: collection.mutable.Map[ru.Symbol, List[(ru.Symbol, StaticAnnotation)]] = collection.mutable.Map.empty

  def findAnnotations(symbol: ru.Symbol, includeAnnotation: ru.Symbol => Boolean): List[StaticAnnotation] = this.synchronized {
    val annotations = annotationCache.getOrElseUpdate(symbol, {
      symbol.annotations.flatMap { annotation =>
        val annotationSymbol: ru.Symbol = annotation.tree.tpe.typeSymbol
        val annotationParams: List[ru.Tree] = annotation.tree.children.tail
        Annotations.parseAnnotation(annotationSymbol, annotationParams)
          .map(staticAnnotation => (annotationSymbol, staticAnnotation))
      }
    })
    annotations.filter(x => includeAnnotation(x._1)).map(_._2)
  }

  import scala.tools.reflect.ToolBox
  private lazy val tb = reflect.runtime.currentMirror.mkToolBox()

  private def unescapeJava(str: Any) = StringEscapeUtils.unescapeJava(str.toString.replaceAll("\"$|^\"", ""))

  private def parseAnnotation(annotationSymbol: ru.Symbol, params: List[ru.Tree]): Option[StaticAnnotation] = {
    if (!annotationSymbol.isClass || !annotationSymbol.asClass.baseClasses.contains(ru.typeOf[StaticAnnotation].typeSymbol)) {
      None
    } else {
      Some(doParseAnnotation(annotationSymbol, params))
    }
  }

  private def doParseAnnotation(annotationSymbol: ru.Symbol, params: List[ru.Tree]): StaticAnnotation = {
    val StringClass = classOf[String]
    val DoubleClass = classOf[Double]
    val IntegerClass = classOf[Int]
    val BooleanClass = classOf[Boolean]

    val annotationClass = Class.forName(annotationSymbol.asClass.fullName)
    val constructor: Constructor[_] = annotationClass.getConstructors.headOption.getOrElse {
      throw new RuntimeException(
        s"Cannot parse annotation ${annotationSymbol.fullName}: scala-schema can only instantiate supported Scala StaticAnnotation classes with public constructors. " +
          "If this is an unexpected Java/JDK annotation, filter it out before parsing annotations."
      )
    }

    def parseAsDouble(v: Any) = Double.box(v.toString.toDouble)
    def parseAsInteger(v: Any) = Integer.valueOf(v.toString.toDouble.toInt)
    def parseAsBoolean(v: Any) = Boolean.box(v.toString.toBoolean)

    def parseAnnotationParam(klass: Class[_], rawValue: ru.Tree): AnyRef = {
      val value = rawValue match {
        case namedArg: ru.NamedArgApi => namedArg.rhs.asInstanceOf[ru.Tree]
        case other => other
      }

      (klass, value) match {
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
