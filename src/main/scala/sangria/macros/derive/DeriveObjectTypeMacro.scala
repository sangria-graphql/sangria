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
              fieldWithArguments(config, field, ctxType, valType, useFn)

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
        else Left(List(c.enclosingPosition → "Field list is empty"))
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

  private def fieldWithArguments(config: Seq[MacroDeriveObjectSetting], member: KnownMember, ctxType: Type, valType: Type, useFn: Boolean) = {
    val args = member.method.paramLists.map(_ map createArg(config, member))
    val argsAst = args map (_ map {
      case NormalArg(name, tpe, _, false) ⇒ q"c.arg[$tpe]($name)"
      case NormalArg(name, tpe, _, true) ⇒ q"c.argOpt[$tpe]($name)"
      case ContextArg ⇒ q"c"
    })

    args.flatten.collect{case na: NormalArg ⇒ na.tree} →
      q"(c: sangria.schema.Context[$ctxType, $valType]) ⇒ ${if (useFn) q"valFn(c.ctx)" else q"c.value"}.${member.method.name}(...$argsAst)"
  }

  private def createArg(config: Seq[MacroDeriveObjectSetting], member: KnownMember)(arg: Symbol) = arg match {
    case term: TermSymbol if term.typeSignature.resultType.erasure =:= typeOf[Context[_, _]].erasure ⇒
      ContextArg
    case term: TermSymbol ⇒
      val tpe = term.typeSignature.resultType
      val methodName = member.method.name.decodedName.toString
      val argName = term.name.decodedName.toString

      val name = collectArgRename(config, methodName, argName) orElse
        symbolName(term.annotations).collect {case q"${s: String}" ⇒ s} getOrElse argName

      val description = collectArgDescription(config, methodName, argName) orElse symbolDescription(term.annotations)
      val default = collectArgDefault(config, methodName, argName) orElse symbolDefault(term.annotations)

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
    tpe.members.collect {
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

    def getMethod(pos: Position, name: String) =
      knownMembers.withFilter(_.name == name).map(_.method) match {
        case method :: Nil ⇒ Right(method)
        case Nil ⇒ Left(unknownMember(pos, name) :: Nil)
        case _ ⇒ Left(List(
          pos → s"Cannot configure overloaded method '$name' using `DeriveObjectSetting` due to ambiguity, use annotations instead."
        ))
      }
    def getArgument(pos: Position, methodName: String, argName: String) = getMethod(pos, methodName).right.flatMap{ method ⇒
      val knownArguments = method.paramLists.flatten
      knownArguments.find(_.name.decodedName.toString == argName)
        .map(Right(_))
        .getOrElse(Left(List(
          pos → s"Unknown argument '$argName' of method '$method'. Known arguments are: ${knownArguments.map(_.name.decodedName) mkString ", "}"
        )))
    }
    def validateHasArgument(pos: Position, methodName: String, argName: String) = getArgument(pos, methodName, argName).right.map(_ ⇒ Nil).merge
    def validateArgumentDefault(pos: Position, methodName: String, argName: String, default: Tree, argType: Type) =
      getArgument(pos, methodName, argName).right.map{
        case arg if argType <:< arg.typeSignature.resultType ⇒ Nil
        case arg ⇒ List(
          pos → s"Wrong type of default value '$default' for argument '$argName' of method '$methodName': expected '${arg.typeSignature.resultType}', but got '$argType'."
        )
      }.merge


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

      case MacroMethodArgumentRename(methodName, argName, _, pos) ⇒
        validateHasArgument(pos, methodName, argName)

      case MacroMethodArgumentDescription(methodName, argName, _, pos) ⇒
        validateHasArgument(pos, methodName, argName)

      case MacroMethodArgumentsDescription(methodName, descriptions, pos) ⇒
        descriptions.keys.toList.flatMap(validateHasArgument(pos, methodName, _))

      case MacroMethodArgumentDefault(methodName, argName, argType, default, pos) ⇒
        validateArgumentDefault(pos, methodName, argName, default, argType)

      case MacroMethodArgument(methodName, argName, _, argType, default, pos) ⇒
        validateArgumentDefault(pos, methodName, argName, default, argType)

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

    case tree @ q"$setting.apply[$_, $_](${methodName: String}, ${argName: String}, ${newName: String})" if checkSetting[MethodArgumentRename.type](setting) ⇒
      Right(MacroMethodArgumentRename(methodName, argName, newName, tree.pos))

    case tree @ q"$setting.apply[$_, $_](${methodName: String}, ${argName: String}, $description)" if checkSetting[MethodArgumentDescription.type](setting) ⇒
      Right(MacroMethodArgumentDescription(methodName, argName, description, tree.pos))

    case tree @ q"$setting.apply[$_, $_](${methodName: String}, ..$descriptions)" if checkSetting[MethodArgumentsDescription.type](setting) ⇒
      val descriptionsMap = descriptions.map{
        case q"(${argName: String}, $description)" ⇒ argName → description
        case q"scala.this.Predef.ArrowAssoc[$_](${argName: String}).->[$_]($description)" ⇒ argName → description // scala 2.11
        case q"scala.Predef.ArrowAssoc[$_](${argName: String}).->[$_]($description)" ⇒ argName → description      // scala 2.12
      }.toMap
      Right(MacroMethodArgumentsDescription(methodName, descriptionsMap, tree.pos))

    case tree @ q"$setting.apply[$_, $_, ${arg: Type}](${methodName: String}, ${argName: String}, $default)" if checkSetting[MethodArgumentDefault.type](setting) ⇒
      Right(MacroMethodArgumentDefault(methodName, argName, arg, default, tree.pos))

    case tree @ q"$setting.apply[$_, $_, ${arg: Type}](${methodName: String}, ${argName: String}, $description, $default)" if checkSetting[MethodArgument.type](setting) ⇒
      Right(MacroMethodArgument(methodName, argName, description, arg, default, tree.pos))

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

  case class MacroMethodArgumentRename(methodName: String, argName: String, newName: String, pos: Position) extends MacroDeriveObjectSetting
  case class MacroMethodArgumentDescription(methodName: String, argName: String, description: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroMethodArgumentsDescription(methodName: String, descriptions: Map[String, Tree], pos: Position) extends MacroDeriveObjectSetting
  case class MacroMethodArgumentDefault(methodName: String, argName: String, defaultType: Type, default: Tree, pos: Position) extends MacroDeriveObjectSetting
  case class MacroMethodArgument(methodName: String, argName: String, description: Tree, defaultType: Type, default: Tree, pos: Position) extends MacroDeriveObjectSetting

  private def collectArgRename(config: Seq[MacroDeriveObjectSetting], methodName: String, argName: String) = config.collect{
    case MacroMethodArgumentRename(`methodName`, `argName`, newName, _) => newName
  }.lastOption

  private def collectArgDescription(config: Seq[MacroDeriveObjectSetting], methodName: String, argName: String) = config.collect{
    case MacroMethodArgumentDescription(`methodName`, `argName`, description, _) ⇒ Some(description)
    case MacroMethodArgumentsDescription(`methodName`, descriptions, _) ⇒ descriptions.get(argName)
    case MacroMethodArgument(`methodName`, `argName`, description, _, _, _) ⇒ Some(description)
  }.flatten.lastOption

  private def collectArgDefault(config: Seq[MacroDeriveObjectSetting], methodName: String, argName: String) = config.collect{
    case MacroMethodArgumentDefault(`methodName`, `argName`, _, default, _) ⇒ default
    case MacroMethodArgument(`methodName`, `argName`, _, _, default, _) ⇒ default
  }.lastOption
}