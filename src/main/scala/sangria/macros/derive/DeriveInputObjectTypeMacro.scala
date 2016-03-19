package sangria.macros.derive

import scala.reflect.macros.blackbox

class DeriveInputObjectTypeMacro(context: blackbox.Context) extends {
  val c = context
} with DeriveMacroSupport {
  import c.universe._

  def deriveInputObjectType[T : WeakTypeTag](config: Tree*) = {
    val targetType = weakTypeTag[T].tpe
    val validatedConfig = validateObjectConfig(config, targetType)

    val errors = validatedConfig.collect {case Left(error) ⇒ error}

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect {case Right(cfg) ⇒ cfg}

      collectFields(validConfig, targetType) match {
        case Left(errors) ⇒ reportErrors(errors)
        case Right(fields) ⇒
          val tpeName = q"${targetType.typeSymbol.name.decodedName.toString}"

          val annotationName = symbolName(targetType.typeSymbol.annotations)
          val configName = validConfig.collect{case MacroName(name) ⇒ name}.lastOption

          val annotationDesc = symbolDescription(targetType.typeSymbol.annotations)
          val configDesc = validConfig.collect{case MacroDescription(name) ⇒ name}.lastOption

          q"""
            sangria.schema.InputObjectType.createFromMacro[$targetType](
              ${configName orElse annotationName getOrElse tpeName},
              ${configDesc orElse annotationDesc},
              () ⇒ ${fields map c.untypecheck})
          """
      }
    }
  }

  private def findApplyMethod(tpe: Type): Either[(Position, String), Option[(Type, MethodSymbol)]] =
    if (tpe.companion =:= NoType) Right(None)
    else {
      val applyMethods = tpe.companion.members.collect {
        case m: MethodSymbol if m.name.decodedName.toString == "apply" ⇒ m
      }

      if (applyMethods.size > 1)
        Left(c.enclosingPosition → "Companion object has more than one `apply` method, which is not supported.")
      else
        Right(Some(tpe.companion → applyMethods.head))
    }

  private def collectFields(config: Seq[MacroSetting], targetType: Type): Either[List[(Position, String)], List[Tree]] =
    findApplyMethod(targetType) match {
      case Right(apply) ⇒
        val knownMembers = findKnownMembers(targetType, apply)

        validateFieldConfig(knownMembers, config) match {
          case Nil ⇒
            val fields = extractFields(knownMembers, config)

            val classFields = fields map { field ⇒
              val fieldType = field.method.returnType

              val name = field.name
              val annotationName = symbolName(field.annotations)
              val configName = config.collect{case MacroRenameField(`name`, tree, _) ⇒ tree}.lastOption

              val annotationDescr = symbolDescription(field.annotations)
              val configDescr = config.collect{case MacroDocumentField(`name`, tree, _) ⇒ tree}.lastOption

              q"""
                sangria.schema.InputField.createFromMacroWithoutDefault(
                  ${configName orElse annotationName getOrElse q"$name"},
                  sangria.macros.derive.GraphQLInputTypeLookup.finder[$fieldType]().graphqlType,
                  ${configDescr orElse annotationDescr})
              """
            }

            val allFields = classFields ++ additionalFields(config)

            if (allFields.nonEmpty) Right(allFields)
            else Left(List(c.enclosingPosition → "Input field list is empty"))
          case errors ⇒ Left(errors)
        }
      case Left(error) ⇒ reportErrors(error :: Nil)
    }

  private def findKnownMembers(tpe: Type, apply: Option[(Type, MethodSymbol)]): List[KnownMember] =
    tpe.members.collect {
      case m: MethodSymbol if m.isCaseAccessor ⇒
        val (annotations, default) = findCaseClassAccessorAnnotations(tpe, m, apply)

        KnownMember(tpe, m, annotations, default)
    }.toList.reverse

  private def findCaseClassAccessorAnnotations(tpe: Type, member: MethodSymbol, applyInfo: Option[(Type, MethodSymbol)]): (List[Annotation], Option[(Type, Tree)]) =
    applyInfo match {
      case Some((companion, apply)) ⇒
        val annotations = for {
          pl ← apply.paramLists
          p ← pl
          if p.name.decodedName.toString == member.name.decodedName.toString
        } yield p.annotations

        annotations.toList.flatten → None // TODO: default
      case None ⇒
        Nil → None
    }

  private def extractFields(knownMembers: List[KnownMember], config: Seq[MacroSetting]) = {
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

  private def additionalFields(config: Seq[MacroSetting]) =
    config.foldLeft(List[Tree]()){
      case (acc, MacroReplaceField(_, field, _)) ⇒ acc :+ field
      case (acc, _) ⇒ acc
    }

  private def validateFieldConfig(knownMembers: List[KnownMember], config: Seq[MacroSetting]) = {
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

      case MacroDocumentField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroRenameField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case MacroReplaceField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) ⇒
        unknownMember(pos, fieldName) :: Nil

      case _ ⇒ Nil
    }
  }

  private def validateObjectConfig(config: Seq[Tree], tpe: Type) = config.map {
    case q"$setting.apply($name)" if checkSetting[InputObjectTypeName.type](setting) ⇒
      Right(MacroName(name))

    case q"$setting.apply($description)" if checkSetting[InputObjectTypeDescription.type](setting) ⇒
      Right(MacroDescription(description))

    case tree @ q"$setting.apply(${fieldName: String}, $description)" if checkSetting[DocumentInputField.type](setting) ⇒
      Right(MacroDocumentField(fieldName, description, tree.pos))

    case tree @ q"$setting.apply(${fieldName: String}, $graphqlName)" if checkSetting[RenameInputField.type](setting) ⇒
      Right(MacroRenameField(fieldName, graphqlName, tree.pos))

    case tree @ q"$setting.apply(${fieldName: String}, $field)" if checkSetting[ReplaceInputField.type](setting) ⇒
      Right(MacroReplaceField(fieldName, field, tree.pos))

    case tree @ q"$setting.apply(..${fields: List[String]})" if checkSetting[IncludeInputFields.type](setting) ⇒
      Right(MacroIncludeFields(fields.toSet, tree.pos))

    case tree @ q"$setting.apply(..${fields: List[String]})" if checkSetting[ExcludeInputFields.type](setting) ⇒
      Right(MacroExcludeFields(fields.toSet, tree.pos))

    case tree ⇒ Left(tree.pos,
      "Unsupported shape of derivation config. " +
        "Please define subclasses of `DeriveInputObjectTypeSetting` directly in the argument list of the macro.")
  }

  private case class KnownMember(onType: Type, method: MethodSymbol, annotations: List[Annotation], defaultValue: Option[(Type, Tree)]) {
    lazy val name = method.name.decodedName.toString
  }

  sealed trait Arg

  case object ContextArg extends Arg
  case class NormalArg(name: String, tpe: Type, tree: Tree) extends Arg

  sealed trait MacroSetting

  case class MacroName(name: Tree) extends MacroSetting
  case class MacroDescription(description: Tree) extends MacroSetting

  case class MacroDocumentField(fieldName: String, description: Tree, pos: Position) extends MacroSetting
  case class MacroRenameField(fieldName: String, graphqlName: Tree, pos: Position) extends MacroSetting
  case class MacroReplaceField(fieldName: String, field: Tree, pos: Position) extends MacroSetting

  case class MacroIncludeFields(fieldNames: Set[String], pos: Position) extends MacroSetting
  case class MacroExcludeFields(fieldNames: Set[String], pos: Position) extends MacroSetting
}