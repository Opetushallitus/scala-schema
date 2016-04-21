package fi.oph.scalaschema

import org.apache.commons.lang3.StringEscapeUtils
import org.reflections.Reflections

import scala.reflect.runtime.{universe => ru}

case class SchemaFactory(annotationsSupported: List[AnnotationSupport[_]] = Nil) {
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

    if (typeName == "scala.Option") {
      // Option[T] becomes the schema of T with required set to false
      OptionalSchema(createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, state))
    } else if (isListType(tpe)) {
      // (Traversable)[T] becomes a schema with items set to the schema of T
      ListSchema(createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, state))
    } else {
      schemaTypeForScala.getOrElse(typeName, {
        if (tpe.typeSymbol.isClass) {
          if (tpe.typeSymbol.isAbstract) {
            addToState(AnyOfSchema(findImplementations(tpe, state), typeName), state)
          } else {
            createClassSchema(tpe, state)
          }
        } else {
          throw new RuntimeException("What is this type: " + tpe)
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

  private def createClassSchema(tpe: ru.Type, state: ScanState) = {
    val traits = findTraits(tpe)

    val className: String = tpe.typeSymbol.fullName
    def ref: ClassRefSchema = applyAnnotations(tpe.typeSymbol, ClassRefSchema(className, Nil))
    if (!state.foundTypes.contains(className)) {
      state.foundTypes.add(className)

      val params: List[ru.Symbol] = tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption.getOrElse(Nil)
      val properties: List[Property] = params.map{ paramSymbol =>
        val term = paramSymbol.asTerm
        val termType = createSchema(term.typeSignature, state.childState)
        val termName: String = term.name.decoded.trim
        val property = applyAnnotations(term, Property(termName, termType, Nil))
        val matchingMethodsFromTraits = traits.flatMap (_.members
          .filter(_.isMethod)
          .filter(_.asTerm.asMethod.name.toString == termName )
        ).map(_.asTerm).distinct
        matchingMethodsFromTraits.foldLeft(property) { (property, traitMethod) =>
          applyAnnotations(traitMethod, property)
        }
      }.toList

      if (state.root) {
        val classTypeDefinitions = state.createdTypes.toList
        applyAnnotations(tpe.typeSymbol, ClassSchema(className, properties, Nil, classTypeDefinitions.sortBy(_.simpleName)))
      } else {
        addToState(applyAnnotations(tpe.typeSymbol, ClassSchema(className, properties, Nil)), state)
        ref
      }
    } else {
      ref
    }
  }

  private def findTraits(tpe: ru.Type) = {
    tpe.baseClasses
      .map(_.fullName)
      .filter(!List("scala.Any").contains(_))
      .map {typeByName(_)}
      .filter {_.typeSymbol.asClass.isTrait}
  }

  private def applyAnnotations[T <: ObjectWithMetadata[T]](symbol: ru.Symbol, x: T): T = {
    symbol.annotations.flatMap(annotation => annotationsSupported.map((annotation, _))).foldLeft(x) { case (current, (annotation, metadataSupport)) =>

      val annotationParams: List[String] = annotation.tree.children.tail.map(str => StringEscapeUtils.unescapeJava(str.toString.replaceAll("\"$|^\"", "")))
      val annotationType: String = annotation.tree.tpe.toString

      metadataSupport.applyAnnotations(annotationType, annotationParams, current, this).asInstanceOf[T]
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
