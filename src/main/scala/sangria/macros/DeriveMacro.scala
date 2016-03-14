package sangria.macros

import sangria.schema._

import scala.reflect.macros.blackbox

class DeriveMacro(context: blackbox.Context) extends {
  val c = context
} with MacroAstLiftable {

  import c.universe._

  def deriveEnumType[T : WeakTypeTag](config: Tree*) = {
    val t = weakTypeTag[T]

    if (t.tpe <:< typeOf[Enumeration#Value]) {
      println("aaa " + collectEnumValues(t.tpe))
    } else {
      collectKnownEnumSubtypes(t.tpe.typeSymbol) match {
        case Left(error) ⇒ reportErrors(error :: Nil)
        case Right(set) ⇒ println(set)
      }
    }



    q"2"
  }

  private def collectEnumValues(tpe: Type): Set[Symbol] =
    tpe.asInstanceOf[TypeRef].pre.members.filter(s ⇒ s.isTerm && !(s.isMethod || s.isModule || s.isClass)).toSet

  private def collectKnownEnumSubtypes(s: Symbol): Either[(Position, String), Set[Symbol]] =
    if (s.isModule || s.isModuleClass) Right(Set(s))
    else if (s.isClass) {
      val cs = s.asClass

      if ((cs.isTrait || cs.isAbstract) && cs.isSealed)
        cs.knownDirectSubclasses.foldLeft(Right(Set.empty): Either[(Position, String), Set[Symbol]]) {
          case (Left(error), _) ⇒ Left(error)
          case (Right(set), knownSubclass) ⇒
            collectKnownEnumSubtypes(knownSubclass) match {
              case Left(error) ⇒ Left(error)
              case Right(subset) ⇒ Right(set ++ subset)
            }
        }
      else Left(cs.pos → "Only `Enumeration` and sealed hierarchies or case objects are supported for GraphQL EnumType derivation.")
    } else Right(Set.empty)

  def deriveObjectType[Ctx : WeakTypeTag, Val : WeakTypeTag](config: Tree*) = {
    val ctx = weakTypeTag[Ctx]
    val v = weakTypeTag[Val]

    val validatedConfig = validateConfig(config)

    val errors = validatedConfig.collect {case Left(error) ⇒ error}

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect {case Right(cfg) ⇒ cfg}

      collectFields(validConfig, ctx.tpe, v.tpe) match {
        case Left(errors) ⇒ reportErrors(errors)
        case Right(fields) ⇒
          val tpeName = q"${v.tpe.typeSymbol.name.decodedName.toString}"

          val annotationName = symbolName(v.tpe.typeSymbol.annotations)
          val configName = validConfig.collect{case MacroName(name) ⇒ name}.lastOption

          val annotationDesc = symbolDescription(v.tpe.typeSymbol.annotations)
          val configDesc = validConfig.collect{case MacroDescription(name) ⇒ name}.lastOption

          val interfaces = validConfig.foldLeft(List[Tree]()) {
            case (acc, MacroInterfaces(tree)) ⇒ acc ++ tree.map(i ⇒ q"$i.interfaceType")
            case (acc, _) ⇒ acc
          }

          q"""
           sangria.schema.ObjectType(
             ${configName orElse annotationName getOrElse tpeName},
             ${configDesc orElse annotationDesc},
             () ⇒ ${fields map c.untypecheck},
             $interfaces)
          """
      }
    }
  }

  private def collectFields(config: Seq[MacroDeriveOutConfig], ctxType: Type, valType: Type): Either[List[(Position, String)], List[Tree]] = {
    val knownMembers = findKnownMembers(valType)

    validateFieldConfig(knownMembers, config) match {
      case Nil ⇒
        val fields = extractFields(knownMembers, config)

        val classFields = fields map { field ⇒
          val fieldName = field.method.name
          val fieldType = field.method.returnType

          val name = field.name
          val annotationName = symbolName(field.annotations)
          val configName = config.collect{case MacroRenameField(`name`, tree, _) ⇒ tree}.lastOption

          val annotationDescr = symbolDescription(field.annotations)
          val configDescr = config.collect{case MacroDocumentField(`name`, tree, _, _) ⇒ tree}.lastOption

          val annotationDepr = symbolDeprecation(field.annotations)
          val configDocDepr = config.collect{case MacroDocumentField(`name`, _, reason, _) ⇒ reason}.lastOption getOrElse q"None"
          val configDepr = config.collect{case MacroDeprecateField(`name`, reason, _) ⇒ reason}.lastOption getOrElse q"None"

          val annotationTags = symbolFieldTags(field.annotations)
          val configTags = config.foldLeft(q"List[sangria.execution.FieldTag]()") {
            case (acc, MacroFieldTags(`name`, tree, _)) ⇒ q"$acc ++ ${tree.toList}"
            case (acc, _) ⇒ acc
          }

          q"""
            Field[$ctxType, $valType, $fieldType, $fieldType](
              ${configName orElse annotationName getOrElse q"$name"},
              implicitly[GraphQLOutputTypeLookup[$fieldType]].graphqlType,
              ${configDescr orElse annotationDescr},
              Nil,
              (c: Context[$ctxType, $valType]) ⇒ c.value.$fieldName,
              Nil,
              $configTags ++ $annotationTags,
              None,
              $configDocDepr orElse $configDepr orElse $annotationDepr)
          """
        }

        Right(classFields ++ additionalFields(config))
      case errors ⇒ Left(errors)
    }
  }

  private def findKnownMembers(tpe: Type): List[KnownMember] =
    tpe.members.collect {
      case m: MethodSymbol if m.isCaseAccessor ⇒ KnownMember(tpe, m, findCaseClassAccessorAnnotations(tpe, m))
    }.toList.reverse

  private def findCaseClassAccessorAnnotations(tpe: Type, member: MethodSymbol): List[Annotation] =
    if (tpe.companion =:= NoType) Nil
    else {
      val applyMethods = tpe.companion.members.collect {
        case m: MethodSymbol if m.name.decodedName.toString == "apply" ⇒ m
      }

      val annotations = for {
        m ← applyMethods
        pl ← m.paramLists
        p ← pl
        if p.name.decodedName.toString == member.name.decodedName.toString
      } yield p.annotations

      annotations.toList.flatten
    }

  private def extractFields(knownMembers: List[KnownMember], config: Seq[MacroDeriveOutConfig]) = {
    val included = config.foldLeft(Set.empty[String]){
      case (acc, MacroIncludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, _) ⇒ acc
    }

    val excluded = config.foldLeft(Set.empty[String]){
      case (acc, MacroExcludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, MacroOverrideField(fieldName, _, _)) ⇒ acc + fieldName
      case (acc, _) ⇒ acc
    }

    val actualIncluded =
      if (included.nonEmpty) included
      else knownMembers.map(m ⇒ m.name).toSet

    val actualFields = actualIncluded -- excluded

    knownMembers.filter(m ⇒ actualFields.contains(m.name) && !memberExcluded(m))
  }

  private def validateFieldConfig(knownMembers: List[KnownMember], config: Seq[MacroDeriveOutConfig]) = {
    val knownMembersSet = knownMembers.map(_.name).toSet

    def unknownMember(pos: Position, name: String) =
      pos → s"Unknown member '$name'. Known members are: ${knownMembers map (_.name) mkString ", "}"

    config.toList.flatMap {
      case MacroIncludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) ⇒
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroExcludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) ⇒
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroDocumentField(fieldName, _, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroRenameField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroFieldTags(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroDeprecateField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroOverrideField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case _ ⇒ Nil
    }
  }

  private def additionalFields(config: Seq[MacroDeriveOutConfig]) =
    config.foldLeft(List[Tree]()){
      case (acc, MacroAddFields(fields)) ⇒ acc ++ fields
      case (acc, MacroOverrideField(_, field, _)) ⇒ acc :+ field
      case (acc, _) ⇒ acc
    }

  private def validateConfig(config: Seq[Tree]) = config.map {
    case q"ObjectTypeName.apply[$_, $_]($name)" ⇒
      Right(MacroName(name))

    case q"ObjectTypeDescription.apply[$_, $_]($description)" ⇒
      Right(MacroDescription(description))

    case q"Interfaces.apply[$_, $_](..$ints)" ⇒
      Right(MacroInterfaces(ints))

    case tree @ q"DocumentField.apply[$_, $_](${fieldName: String}, $description, $deprecationReason)" ⇒
      Right(MacroDocumentField(fieldName, description, deprecationReason, tree.pos))

    case tree @ q"RenameField.apply[$_, $_](${fieldName: String}, $graphqlName)" ⇒
      Right(MacroRenameField(fieldName, graphqlName, tree.pos))

    case tree @ q"FieldTags.apply[$_, $_](${fieldName: String}, ..$fieldTags)" ⇒
      Right(MacroFieldTags(fieldName, fieldTags, tree.pos))

    case tree @ q"DeprecateField.apply[$_, $_](${fieldName: String}, $deprecationReason)" ⇒
      Right(MacroDeprecateField(fieldName, q"Some($deprecationReason)", tree.pos))

    case tree @ q"IncludeFields.apply[$_, $_](..${fields: List[String]})" ⇒
      Right(MacroIncludeFields(fields.toSet, tree.pos))

    case tree @ q"ExcludeFields.apply[$_, $_](..${fields: List[String]})" ⇒
      Right(MacroExcludeFields(fields.toSet, tree.pos))

    case q"AddFields.apply[$_, $_](..$fields)" ⇒
      Right(MacroAddFields(fields))

    case tree @ q"OverrideField.apply[$_, $_](${fieldName: String}, $field)" ⇒
      Right(MacroOverrideField(fieldName, field, tree.pos))

    case tree ⇒ Left(tree.pos,
      "Unsupported shape of derivation config. " +
          "Please define subclasses of `DeriveConfig` directly in the argument list of the macro.")
  }

  private def reportErrors(errors: Seq[(Position, String)]) = {
    require(errors.nonEmpty)

    val (lastPos, lastError) = errors.last

    errors.dropRight(1).foreach{case (pos, error) ⇒ c.error(pos, error)}

    c.abort(lastPos, lastError)
  }

  private def symbolName(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLName] ⇒ arg}
      .headOption

  private def symbolDescription(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDescription] ⇒ arg}
      .headOption

  private def symbolDeprecation(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDeprecated] ⇒ arg}
      .headOption

  private def symbolFieldTags(annotations: List[Annotation]): Tree =
    annotations
      .map (_.tree)
      .foldLeft(q"List[sangria.execution.FieldTag]()") {
        case (acc, q"new $name(..$fieldTags)") if name.tpe =:= typeOf[GraphQLFieldTags] ⇒
          q"$acc ++ $fieldTags"
        case (acc, _) ⇒ acc
      }

  private def memberExcluded(member: KnownMember): Boolean =
    member.annotations.find(_.tree.tpe =:= typeOf[GraphQLExclude]).fold(false)(_ ⇒ true)

  private case class KnownMember(onType: Type, method: MethodSymbol, annotations: List[Annotation]) {
    lazy val name = method.name.decodedName.toString
  }

  sealed trait MacroDeriveOutConfig

  case class MacroName(name: Tree) extends MacroDeriveOutConfig
  case class MacroDescription(description: Tree) extends MacroDeriveOutConfig
  case class MacroInterfaces(interfaces: Seq[Tree]) extends MacroDeriveOutConfig

  case class MacroDocumentField(fieldName: String, description: Tree, deprecationReason: Tree, pos: Position) extends MacroDeriveOutConfig
  case class MacroRenameField(fieldName: String, graphqlName: Tree, pos: Position) extends MacroDeriveOutConfig
  case class MacroFieldTags(fieldName: String, tags: Seq[Tree], pos: Position) extends MacroDeriveOutConfig
  case class MacroDeprecateField(fieldName: String, deprecationReason: Tree, pos: Position) extends MacroDeriveOutConfig

  case class MacroIncludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveOutConfig
  case class MacroExcludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveOutConfig
  case class MacroAddFields(fields: List[Tree]) extends MacroDeriveOutConfig
  case class MacroOverrideField(fieldName: String, field: Tree, pos: Position) extends MacroDeriveOutConfig
}