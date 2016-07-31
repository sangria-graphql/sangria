package sangria.macros.derive

import sangria.macros._

import scala.reflect.macros.blackbox

class DeriveEnumTypeMacro(context: blackbox.Context) extends {
  val c = context
} with DeriveMacroSupport {
  import c.universe._

  def deriveEnumType[T : WeakTypeTag](config: Tree*) = {
    val t = weakTypeTag[T]
    val validatedConfig = validateEnumConfig(config)
    val errors = validatedConfig.collect {case Left(error) ⇒ error}

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect {case Right(cfg) ⇒ cfg}

      val (tpe, validatedValues) =
        if (t.tpe <:< typeOf[Enumeration#Value])
          t.tpe.asInstanceOf[TypeRef].pre → Right(collectEnumerationValues(t.tpe))
        else
          t.tpe → collectKnownEnumSubtypes(t.tpe.typeSymbol)

      validatedValues match {
        case Left(error) ⇒ reportErrors(error :: Nil)
        case Right(values) ⇒
          validateEnumValueConfig(values, validConfig) match {
            case Nil ⇒
              val tpeName = q"${tpe.typeSymbol.name.decodedName.toString}"

              val annotationName = symbolName(tpe.typeSymbol.annotations)
              val configName = validConfig.collect{case MacroEnumTypeName(name) ⇒ name}.lastOption

              val annotationDesc = symbolDescription(tpe.typeSymbol.annotations)
              val configDesc = validConfig.collect{case MacroEnumTypeDescription(name) ⇒ name}.lastOption

              val enumValues = collectEnumValues(values, validConfig, t.tpe)

              q"""
                sangria.schema.EnumType(
                  ${configName orElse annotationName getOrElse tpeName},
                  ${configDesc orElse annotationDesc},
                  $enumValues)
              """
            case configErrors ⇒ reportErrors(configErrors)
          }

      }
    }
  }

  private def collectEnumerationValues(tpe: Type): List[Symbol] =
    tpe.asInstanceOf[TypeRef].pre.members
      .filter(s ⇒ s.isTerm && !(s.isMethod || s.isModule || s.isClass) && (s.typeSignature.resultType <:< typeOf[Enumeration#Value]))
      .toList

  private def collectKnownEnumSubtypes(s: Symbol): Either[(Position, String), List[Symbol]] =
    if (s.isModule || s.isModuleClass) Right(List(s))
    else if (s.isClass) {
      val cs = s.asClass

      if ((cs.isTrait || cs.isAbstract) && cs.isSealed)
        cs.knownDirectSubclasses.foldLeft(Right(Nil): Either[(Position, String), List[Symbol]]) {
          case (Left(error), _) ⇒ Left(error)
          case (Right(set), knownSubclass) ⇒
            collectKnownEnumSubtypes(knownSubclass) match {
              case Left(error) ⇒ Left(error)
              case Right(subset) ⇒ Right(set ++ subset)
            }
        }
      else Left(cs.pos → "Only `Enumeration` and sealed hierarchies or case objects are supported for GraphQL EnumType derivation.")
    } else Left(c.enclosingPosition → "Only `Enumeration` and sealed hierarchies or case objects are supported for GraphQL EnumType derivation.")

  private def collectEnumValues(values: List[Symbol], config: Seq[MacroDeriveEnumSetting], t: Type): List[Tree] = {
    val extractedValues = extractEnumValues(values, config)

    if (extractedValues.isEmpty) reportErrors(List(c.enclosingPosition → "Enum value list is empty"))
    else
      extractedValues map { value ⇒
        val name = value.name.decodedName.toString.trim
        val annotationName = symbolName(value.annotations)
        val configName = config.collect{case MacroRenameValue(`name`, tree, _) ⇒ tree}.lastOption
        val actualName =
          if (config.exists(_.isInstanceOf[MacroUppercaseValues]))
            q"sangria.util.StringUtil.camelCaseToUnderscore(${configName orElse annotationName getOrElse q"$name"}).toUpperCase"
          else
            q"${configName orElse annotationName getOrElse q"$name"}"

        val annotationDescr = symbolDescription(value.annotations)
        val configDescr = config.collect{case MacroDocumentValue(`name`, tree, _, _) ⇒ tree}.lastOption

        val annotationDepr = symbolDeprecation(value.annotations)
        val configDocDepr = config.collect{case MacroDocumentValue(`name`, _, reason, _) ⇒ reason}.lastOption getOrElse q"None"
        val configDepr = config.collect{case MacroDeprecateValue(`name`, reason, _) ⇒ reason}.lastOption getOrElse q"None"

        val actualValue = {
          if (value.isModuleClass) {
            if (value.owner.isModuleClass) {
              q"${value.owner.name.toTermName}.${value.name.toTermName}"
            } else {
              q"${value.name.toTermName}"
            }
          } else q"${t.asInstanceOf[TypeRef].pre.typeSymbol.name.toTermName}.${value.asTerm.getter}"
        }

        q"""
          sangria.schema.EnumValue[$t](
            $actualName,
            ${configDescr orElse annotationDescr},
            $actualValue,
            $configDocDepr orElse $configDepr orElse $annotationDepr)
        """
      }
  }

  private def extractEnumValues(values: List[Symbol], config: Seq[MacroDeriveEnumSetting]) = {
    val included = config.foldLeft(Set.empty[String]){
      case (acc, MacroIncludeValues(vals, _)) ⇒ acc ++ vals
      case (acc, _) ⇒ acc
    }

    val excluded = config.foldLeft(Set.empty[String]){
      case (acc, MacroExcludeValues(vals, _)) ⇒ acc ++ vals
      case (acc, _) ⇒ acc
    }

    val actualIncluded =
      if (included.nonEmpty) included
      else values.map(m ⇒ m.name.decodedName.toString.trim).toSet

    val actualFields = actualIncluded -- excluded

    values.filter(m ⇒ actualFields.contains(m.name.decodedName.toString.trim) && !memberExcluded(m.annotations))
  }

  private def validateEnumValueConfig(knownMembers: List[Symbol], config: Seq[MacroDeriveEnumSetting]) = {
    val knownMembersSet = knownMembers.map(_.name.decodedName.toString.trim).toSet

    def unknownMember(pos: Position, name: String) =
      pos → s"Unknown enum value '$name'. Known members are: ${knownMembers map (_.name.decodedName.toString) mkString ", "}"

    val valueValidations = config.toList.flatMap {
      case MacroIncludeValues(values, pos) if !values.forall(knownMembersSet.contains) ⇒
        val unknown = values.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroExcludeValues(values, pos) if !values.forall(knownMembersSet.contains) ⇒
        val unknown = values.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroDocumentValue(value, _, _, pos) if !knownMembersSet.contains(value) ⇒
        unknownMember(pos, value) :: Nil

      case MacroRenameValue(value, _, pos) if !knownMembersSet.contains(value) ⇒
        unknownMember(pos, value) :: Nil

      case MacroDeprecateValue(value, _, pos) if !knownMembersSet.contains(value) ⇒
        unknownMember(pos, value) :: Nil

      case _ ⇒ Nil
    }

    config.collect{case MacroUppercaseValues(pos) ⇒ pos}.lastOption match {
      case Some(pos) if config.exists(_.isInstanceOf[MacroRenameValue]) ⇒
        valueValidations :+ (pos → "`UppercaseValues` is used together with `RenameValue` which is not allowed.")
      case _ ⇒
        valueValidations
    }
  }

  private def validateEnumConfig(config: Seq[Tree]) = config.map {
    case q"$setting.apply($name)" if checkSetting[EnumTypeName.type](setting) ⇒
      Right(MacroEnumTypeName(name))

    case q"$setting.apply($description)" if checkSetting[EnumTypeDescription.type](setting) ⇒
      Right(MacroEnumTypeDescription(description))

    case tree @ q"$setting" if checkSetting[UppercaseValues.type](setting) ⇒
      Right(MacroUppercaseValues(tree.pos))

    case tree @ q"$setting.apply(${value: String}, $description, $deprecationReason)" if checkSetting[DocumentValue.type](setting) ⇒
      Right(MacroDocumentValue(value, description, deprecationReason, tree.pos))

    case tree @ q"$setting.apply(${value: String}, $graphqlName)" if checkSetting[RenameValue.type](setting) ⇒
      Right(MacroRenameValue(value, graphqlName, tree.pos))

    case tree @ q"$setting.apply(${value: String}, $deprecationReason)" if checkSetting[DeprecateValue.type](setting) ⇒
      Right(MacroDeprecateValue(value, q"Some($deprecationReason)", tree.pos))

    case tree @ q"$setting.apply(..${values: List[String]})" if checkSetting[IncludeValues.type](setting) ⇒
      Right(MacroIncludeValues(values.toSet, tree.pos))

    case tree @ q"$setting.apply(..${values: List[String]})" if checkSetting[ExcludeValues.type](setting) ⇒
      Right(MacroExcludeValues(values.toSet, tree.pos))

    case tree ⇒ Left(tree.pos →
        "Unsupported shape of derivation config. Please define subclasses of `DeriveEnumTypeConfig` directly in the argument list of the macro.")
  }

  sealed trait MacroDeriveEnumSetting

  case class MacroEnumTypeName(name: Tree) extends MacroDeriveEnumSetting
  case class MacroEnumTypeDescription(description: Tree) extends MacroDeriveEnumSetting

  case class MacroUppercaseValues(pos: Position) extends MacroDeriveEnumSetting

  case class MacroDocumentValue(value: String, description: Tree, deprecationReason: Tree, pos: Position) extends MacroDeriveEnumSetting
  case class MacroDeprecateValue(value: String, deprecationReason: Tree, pos: Position) extends MacroDeriveEnumSetting
  case class MacroRenameValue(value: String, graphqlName: Tree, pos: Position) extends MacroDeriveEnumSetting

  case class MacroIncludeValues(values: Set[String], pos: Position) extends MacroDeriveEnumSetting
  case class MacroExcludeValues(fieldNames: Set[String], pos: Position) extends MacroDeriveEnumSetting
}