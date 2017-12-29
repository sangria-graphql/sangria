package sangria.macros.derive

import sangria.execution.deferred.Deferred
import sangria.schema.{Action, Context}

import scala.concurrent.Future
import scala.reflect.macros.blackbox

class DeriveObjectTypeMacro(context: blackbox.Context) extends {
  val c = context
} with DeriveMacroSupport {
  import c.universe._

  def deriveContextObjectType[Ctx : WeakTypeTag, CtxVal : WeakTypeTag, Val : WeakTypeTag](fn: Tree, config: Tree*) = {
    val ctx = weakTypeTag[Ctx]
    val ctxVal = weakTypeTag[CtxVal]
    val v = weakTypeTag[Val]

    deriveObjectType(ctx.tpe, Some(ctxVal.tpe → fn), v.tpe, config)
  }

  def deriveNormalObjectType[Ctx : WeakTypeTag, Val : WeakTypeTag](config: Tree*) = {
    val ctx = weakTypeTag[Ctx]
    val v = weakTypeTag[Val]

    deriveObjectType(ctx.tpe, None, v.tpe, config)
  }

  def deriveObjectType(ctxType: Type, ctxValType: Option[(Type, Tree)], valType: Type, config: Seq[Tree]) = {
    val targetType = ctxValType.fold(valType)(_._1)
    val validatedConfig = validateObjectConfig(config, targetType)

    val errors = validatedConfig.collect {case Left(error) ⇒ error}

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect {case Right(cfg) ⇒ cfg}

      collectFields(validConfig, ctxType, targetType, valType, ctxValType.isDefined) match {
        case Left(errors) ⇒ reportErrors(errors)
        case Right(fields) ⇒
          val tpeName = q"${targetType.typeSymbol.name.decodedName.toString}"

          val annotationName = symbolName(targetType.typeSymbol.annotations)
          val configName = validConfig.collect{case MacroName(name) ⇒ name}.lastOption

          val annotationDesc = symbolDescription(targetType.typeSymbol.annotations)
          val configDesc = validConfig.collect{case MacroDescription(name) ⇒ name}.lastOption

          val interfaces = validConfig.foldLeft(List[Tree]()) {
            case (acc, MacroInterfaces(tree)) ⇒ acc ++ tree.map(i ⇒ q"$i.interfaceType")
            case (acc, _) ⇒ acc
          }

          q"""
            ${ctxValType.fold(q"")(cv ⇒ q"val valFn = ${cv._2}")}

            sangria.schema.ObjectType.createFromMacro(
              ${configName orElse annotationName getOrElse tpeName},
              ${configDesc orElse annotationDesc},
              $interfaces,
              () ⇒ ${fields map c.untypecheck})
          """
      }
    }
  }

  private def collectFields(config: Seq[MacroDeriveObjectSetting], ctxType: Type, targetType: Type, valType: Type, useFn: Boolean): Either[List[(Position, String)], List[Tree]] = {
    val knownMembers = findKnownMembers(targetType, config.foldLeft(Set.empty[String]) {
      case (acc, MacroIncludeMethods(methods)) ⇒ acc ++ methods
      case (acc, _) ⇒ acc
    })

    validateFieldConfig(knownMembers, config) match {
      case Nil ⇒
        val fields = extractFields(knownMembers, config)

        val classFields = fields map { field ⇒
          val (args, resolve) =
            if (field.accessor)
              Nil → q"(c: sangria.schema.Context[$ctxType, $valType]) ⇒ ${if (useFn) q"valFn(c.ctx)" else q"c.value"}.${field.method.name}"
            else
              fieldWithArguments(field, ctxType, valType, useFn)

          val fieldType = field.method.returnType
          val actualFieldType = findActualFieldType(fieldType)

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

          val fieldName: c.universe.Tree = {
            val nonTransformedName = configName orElse annotationName getOrElse q"$name"

            config.collect{case MacroTransformFieldNames(fnt) ⇒ fnt}.lastOption match {
              case Some(fnt) ⇒ q"$fnt($nonTransformedName)"
              case None ⇒ nonTransformedName
            }
          }

          q"""
            sangria.schema.Field[$ctxType, $valType, $actualFieldType, $actualFieldType](
              $fieldName,
              implicitly[sangria.macros.derive.GraphQLOutputTypeLookup[$actualFieldType]].graphqlType,
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
        else Left(List(c.enclosingPosition → s"$targetType: Field list is empty"))
      case errors ⇒ Left(errors)
    }
  }

  private def findActualFieldType(fieldType: Type) =
    if (isSupertype[Future[_]](fieldType) && fieldType.typeArgs.nonEmpty)
      fieldType.typeArgs.head
    else if (isSupertype[scala.util.Try[_]](fieldType)  && fieldType.typeArgs.nonEmpty)
      fieldType.typeArgs.head
    else if (isSupertype[Deferred[_]](fieldType) && fieldType.baseType(typeOf[Deferred[_]].typeSymbol).typeArgs.nonEmpty)
      fieldType.baseType(typeOf[Deferred[_]].typeSymbol).typeArgs.head
    else if (isSupertype[Action[_, _]](fieldType) && fieldType.baseType(typeOf[Action[_, _]].typeSymbol).typeArgs.size == 2)
      fieldType.baseType(typeOf[Action[_, _]].typeSymbol).typeArgs(1)
    else
      fieldType

  private def isSupertype[T : TypeTag](subtype: Type) =
    subtype.erasure <:< typeTag[T].tpe.erasure

  private def fieldWithArguments(member: KnownMember, ctxType: Type, valType: Type, useFn: Boolean) = {
    val args = member.method.paramLists.map(_ map createArg)
    val argsAst = args map (_ map {
      case NormalArg(name, tpe, _, false) ⇒ q"c.arg[$tpe]($name)"
      case NormalArg(name, tpe, _, true) ⇒ q"c.argOpt[$tpe]($name)"
      case ContextArg ⇒ q"c"
    })

    args.flatten.collect{case na: NormalArg ⇒ na.tree} →
      q"(c: sangria.schema.Context[$ctxType, $valType]) ⇒ ${if (useFn) q"valFn(c.ctx)" else q"c.value"}.${member.method.name}(...$argsAst)"
  }

  private def createArg(arg: Symbol) = arg match {
    case term: TermSymbol if term.typeSignature.resultType.erasure =:= typeOf[Context[_, _]].erasure ⇒
      ContextArg
    case term: TermSymbol ⇒
      val tpe = term.typeSignature.resultType
      val name = symbolName(term.annotations).collect {case q"${s: String}" ⇒ s} getOrElse term.name.decodedName.toString
      val description = symbolDescription(term.annotations)
      val default = symbolDefault(term.annotations)

      val ast = default match {
        case Some(defaultValue) ⇒
          q"""
            sangria.schema.Argument.createWithDefault(
              $name,
              sangria.schema.OptionInputType(sangria.macros.derive.GraphQLInputTypeLookup.finder[$tpe]().graphqlType),
              $description,
              $defaultValue)
          """
        case None ⇒
          q"""
            sangria.schema.Argument.createWithoutDefault(
              $name,
              sangria.macros.derive.GraphQLInputTypeLookup.finder[$tpe]().graphqlType,
              $description)
          """
      }

      val optional = default.isEmpty && isSupertype[Option[_]](tpe)

      val targetType =
        if (optional)
          tpe.baseType(typeOf[Option[_]].typeSymbol).typeArgs.head
        else
          tpe

      NormalArg(name, targetType, ast, optional)
  }

  private def findKnownMembers(tpe: Type, includeMethods: Set[String]): List[KnownMember] =
    // we "force" m by calling info. This makes sure its type information is complete, in particular that
    // annotations are available through `.annotations`
    tpe.members.map { m => m.info; m }.collect {
      case m: MethodSymbol if m.isCaseAccessor ⇒
        KnownMember(tpe, m, findCaseClassAccessorAnnotations(tpe, m), accessor = true)
      case m: MethodSymbol if memberField(m.annotations) || includeMethods.contains(m.name.decodedName.toString) ⇒
        KnownMember(tpe, m, m.annotations, accessor = false)
      case value: TermSymbol if value.isVal && (memberField(value.annotations) || includeMethods.contains(value.name.decodedName.toString)) ⇒
        KnownMember(tpe, value.getter.asMethod, value.annotations, accessor = false)
    }.toList.reverse

  private def findCaseClassAccessorAnnotations(tpe: Type, member: MethodSymbol): List[Annotation] =
    if (tpe.companion =:= NoType) Nil
    else {
      val annotationsConstructors =
        for {
          c ← tpe.members.filter(_.isConstructor)
          pl ← c.asMethod.paramLists
          p ← pl
          if p.name.decodedName.toString == member.name.decodedName.toString
        } yield p.annotations

      annotationsConstructors.toList.flatten
    }

  private def extractFields(knownMembers: List[KnownMember], config: Seq[MacroDeriveObjectSetting]) = {
    val included = config.foldLeft(Set.empty[String]){
      case (acc, MacroIncludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, _) ⇒ acc
    }

    val excluded = config.foldLeft(Set.empty[String]){
      case (acc, MacroExcludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, MacroReplaceField(fieldName, _, _)) ⇒ acc + fieldName
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

      case MacroReplaceField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case _ ⇒ Nil
    }
  }

  private def additionalFields(config: Seq[MacroDeriveObjectSetting]) =
    config.foldLeft(List[Tree]()){
      case (acc, MacroAddFields(fields)) ⇒ acc ++ fields
      case (acc, MacroReplaceField(_, field, _)) ⇒ acc :+ field
      case (acc, _) ⇒ acc
    }

  private def validateObjectConfig(config: Seq[Tree], tpe: Type) = config.map {
    case q"$setting.apply[$_, $_]($name)" if checkSetting[ObjectTypeName.type](setting) ⇒
      Right(MacroName(name))

    case q"$setting.apply[$_, $_]($description)" if checkSetting[ObjectTypeDescription.type](setting) ⇒
      Right(MacroDescription(description))

    case q"$setting.apply[$_, $_](..$ints)" if checkSetting[Interfaces.type](setting) ⇒
      Right(MacroInterfaces(ints))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, $description, $deprecationReason)" if checkSetting[DocumentField.type](setting) ⇒
      Right(MacroDocumentField(fieldName, description, deprecationReason, tree.pos))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, $graphqlName)" if checkSetting[RenameField.type](setting) ⇒
      Right(MacroRenameField(fieldName, graphqlName, tree.pos))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, ..$fieldTags)" if checkSetting[FieldTags.type](setting) ⇒
      Right(MacroFieldTags(fieldName, fieldTags, tree.pos))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, $deprecationReason)" if checkSetting[DeprecateField.type](setting) ⇒
      Right(MacroDeprecateField(fieldName, q"Some($deprecationReason)", tree.pos))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, $complexity)" if checkSetting[FieldComplexity.type](setting) ⇒
      Right(MacroFieldComplexity(fieldName, complexity, tree.pos))

    case tree @ q"$setting.apply[$_, $_](..${fields: List[String]})" if checkSetting[IncludeFields.type](setting) ⇒
      Right(MacroIncludeFields(fields.toSet, tree.pos))

    case tree @ q"$setting.apply[$_, $_](..${methods: List[String]})" if checkSetting[IncludeMethods.type](setting) ⇒
      val known = tpe.members.collect {case m: MethodSymbol ⇒ m.name.decodedName.toString}.toSet
      val unknown = methods filterNot known.contains

      if (unknown.isEmpty) Right(MacroIncludeMethods(methods.toSet))
      else Left(tree.pos → s"Unknown members: ${unknown mkString ", "}. Known members are: ${known mkString ", "}")

    case tree @ q"$setting.apply[$_, $_](..${fields: List[String]})" if checkSetting[ExcludeFields.type](setting) ⇒
      Right(MacroExcludeFields(fields.toSet, tree.pos))

    case q"$setting.apply[$_, $_](..$fields)" if checkSetting[AddFields.type](setting) ⇒
      Right(MacroAddFields(fields))

    case tree @ q"$setting.apply[$_, $_](${fieldName: String}, $field)" if checkSetting[ReplaceField.type](setting) ⇒
      Right(MacroReplaceField(fieldName, field, tree.pos))

    case q"$setting.apply[$_, $_]($fn)" if checkSetting[TransformFieldNames.type](setting) ⇒
      Right(MacroTransformFieldNames(fn))

    case tree ⇒ Left(tree.pos →
      "Unsupported shape of derivation config. Please define subclasses of `DeriveObjectTypeSetting` directly in the argument list of the macro.")
  }

  private case class KnownMember(onType: Type, method: MethodSymbol, annotations: List[Annotation], accessor: Boolean) {
    lazy val name = method.name.decodedName.toString
  }

  sealed trait Arg

  case object ContextArg extends Arg
  case class NormalArg(name: String, tpe: Type, tree: Tree, optional : Boolean) extends Arg

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
  case class MacroReplaceField(fieldName: String, field: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroTransformFieldNames(transformer: Tree) extends MacroDeriveObjectSetting
}
