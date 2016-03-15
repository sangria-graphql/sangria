package sangria.macros.derive

import sangria.macros._
import sangria.schema.Context

import scala.reflect.macros.blackbox

class DeriveObjectTypeMacro(context: blackbox.Context) extends {
  val c = context
} with DeriveMacroSupport {
  import c.universe._

  def deriveObjectType[Ctx : WeakTypeTag, Val : WeakTypeTag](config: Tree*) = {
    val ctx = weakTypeTag[Ctx]
    val v = weakTypeTag[Val]

    val validatedConfig = validateObjectConfig(config, v.tpe)

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

  private def collectFields(config: Seq[MacroDeriveObjectSetting], ctxType: Type, valType: Type): Either[List[(Position, String)], List[Tree]] = {
    val knownMembers = findKnownMembers(valType, config.foldLeft(Set.empty[String]) {
      case (acc, MacroIncludeMethods(methods)) ⇒ acc ++ methods
      case (acc, _) ⇒ acc
    })

    validateFieldConfig(knownMembers, config) match {
      case Nil ⇒
        val fields = extractFields(knownMembers, config)

        val classFields = fields map { field ⇒
          val (args, resolve) =
            if (field.accessor)
              Nil → q"(c: Context[$ctxType, $valType]) ⇒ c.value.${field.method.name}"
            else
              fieldWithArguments(field, ctxType, valType)

          val fieldType = field.method.returnType

          val name = field.name
          val annotationName = symbolName(field.annotations)
          val configName = config.collect{case MacroRenameField(`name`, tree, _) ⇒ tree}.lastOption

          val annotationDescr = symbolDescription(field.annotations)
          val configDescr = config.collect{case MacroDocumentField(`name`, tree, _, _) ⇒ tree}.lastOption

          val annotationDepr = symbolDeprecation(field.annotations)
          val configDocDepr = config.collect{case MacroDocumentField(`name`, _, reason, _) ⇒ reason}.lastOption getOrElse q"None"
          val configDepr = config.collect{case MacroDeprecateField(`name`, reason, _) ⇒ reason}.lastOption getOrElse q"None"

          val complexity = config.collect{case MacroFieldComplexity(`name`, c, _) ⇒ c}.lastOption

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
              $args,
              $resolve,
              Nil,
              $configTags ++ $annotationTags,
              $complexity,
              $configDocDepr orElse $configDepr orElse $annotationDepr)
          """
        }

        val allFields = classFields ++ additionalFields(config)

        if (allFields.nonEmpty) Right(allFields)
        else Left(List(c.enclosingPosition → "Field list is empty"))
      case errors ⇒ Left(errors)
    }
  }

  private def fieldWithArguments(member: KnownMember, ctxType: Type, valType: Type) = {
    val args = member.method.paramLists.map(list ⇒ list map createArg)
    val argsAst = args map (_ map {
      case NormalArg(name, tpe, _) ⇒ q"c.arg[$tpe]($name)"
      case ContextArg ⇒ q"c"
    })

    args.flatten.collect{case na: NormalArg ⇒ na.tree} →
      q"(c: Context[$ctxType, $valType]) ⇒ c.value.${member.method.name}(...$argsAst)"
  }

  private def createArg(arg: Symbol) = arg match {
    case term: TermSymbol if term.typeSignature.resultType.erasure =:= typeOf[Context[_, _]].erasure ⇒
      ContextArg
    case term: TermSymbol ⇒
      val tpe = term.typeSignature.resultType
      val name = symbolName(term.annotations).collect {case q"${s: String}" ⇒ s} getOrElse term.name.decodedName.toString
      val description = symbolDescription(term.annotations)

      val ast = description match{
        case Some(descr) ⇒ q"sangria.schema.Argument($name, GraphQLInputTypeLookup.foo[$tpe]().graphqlType, $descr)"
        case None ⇒ q"sangria.schema.Argument($name, GraphQLInputTypeLookup.foo[$tpe]().graphqlType)"
      }

      NormalArg(name, tpe, ast)
  }

  private def findKnownMembers(tpe: Type, includeMethods: Set[String]): List[KnownMember] =
    tpe.members.collect {
      case m: MethodSymbol if m.isCaseAccessor ⇒
        KnownMember(tpe, m, findCaseClassAccessorAnnotations(tpe, m), accessor = true)
      case m: MethodSymbol if memberField(m.annotations) || includeMethods.contains(m.name.decodedName.toString) ⇒
        KnownMember(tpe, m, m.annotations, accessor = false)
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

  private def extractFields(knownMembers: List[KnownMember], config: Seq[MacroDeriveObjectSetting]) = {
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

    knownMembers.filter(m ⇒ actualFields.contains(m.name) && !memberExcluded(m.annotations))
  }

  private def validateFieldConfig(knownMembers: List[KnownMember], config: Seq[MacroDeriveObjectSetting]) = {
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

      case MacroFieldComplexity(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroOverrideField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case _ ⇒ Nil
    }
  }

  private def additionalFields(config: Seq[MacroDeriveObjectSetting]) =
    config.foldLeft(List[Tree]()){
      case (acc, MacroAddFields(fields)) ⇒ acc ++ fields
      case (acc, MacroOverrideField(_, field, _)) ⇒ acc :+ field
      case (acc, _) ⇒ acc
    }

  private def validateObjectConfig(config: Seq[Tree], tpe: Type) = config.map {
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

    case tree @ q"FieldComplexity.apply[$_, $_](${fieldName: String}, $complexity)" ⇒
      Right(MacroFieldComplexity(fieldName, complexity, tree.pos))

    case tree @ q"IncludeFields.apply[$_, $_](..${fields: List[String]})" ⇒
      Right(MacroIncludeFields(fields.toSet, tree.pos))

    case tree @ q"IncludeMethods.apply[$_, $_](..${methods: List[String]})" ⇒
      val known = tpe.members.collect {case m: MethodSymbol ⇒ m.name.decodedName.toString}.toSet
      val unknown = methods filterNot known.contains

      if (unknown.isEmpty) Right(MacroIncludeMethods(methods.toSet))
      else Left(tree.pos → s"Unknown members: ${unknown mkString ", "}. Known members are: ${known mkString ", "}")

    case tree @ q"ExcludeFields.apply[$_, $_](..${fields: List[String]})" ⇒
      Right(MacroExcludeFields(fields.toSet, tree.pos))

    case q"AddFields.apply[$_, $_](..$fields)" ⇒
      Right(MacroAddFields(fields))

    case tree @ q"OverrideField.apply[$_, $_](${fieldName: String}, $field)" ⇒
      Right(MacroOverrideField(fieldName, field, tree.pos))

    case tree ⇒ Left(tree.pos,
      "Unsupported shape of derivation config. " +
          "Please define subclasses of `DeriveObjectTypeConfig` directly in the argument list of the macro.")
  }

  private case class KnownMember(onType: Type, method: MethodSymbol, annotations: List[Annotation], accessor: Boolean) {
    lazy val name = method.name.decodedName.toString
  }

  sealed trait Arg

  case object ContextArg extends Arg
  case class NormalArg(name: String, tpe: Type, tree: Tree) extends Arg

  sealed trait MacroDeriveObjectSetting

  case class MacroName(name: Tree) extends MacroDeriveObjectSetting
  case class MacroDescription(description: Tree) extends MacroDeriveObjectSetting
  case class MacroInterfaces(interfaces: Seq[Tree]) extends MacroDeriveObjectSetting

  case class MacroDocumentField(fieldName: String, description: Tree, deprecationReason: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroRenameField(fieldName: String, graphqlName: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroFieldTags(fieldName: String, tags: Seq[Tree], pos: Position) extends MacroDeriveObjectSetting
  case class MacroDeprecateField(fieldName: String, deprecationReason: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroFieldComplexity(fieldName: String, complexity: Tree, pos: Position) extends MacroDeriveObjectSetting

  case class MacroIncludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveObjectSetting
  case class MacroIncludeMethods(methodNames: Set[String]) extends MacroDeriveObjectSetting
  case class MacroExcludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveObjectSetting
  case class MacroAddFields(fields: List[Tree]) extends MacroDeriveObjectSetting
  case class MacroOverrideField(fieldName: String, field: Tree, pos: Position) extends MacroDeriveObjectSetting
}