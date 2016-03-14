package sangria.macros

import sangria.schema._

import scala.reflect.macros.blackbox

class DeriveMacro(context: blackbox.Context) extends {
  val c = context
} with MacroAstLiftable {

  import c.universe._

  def deriveObjectTypeNoArg[Ctx : WeakTypeTag, Val : WeakTypeTag] =
    deriveObjectType[Ctx, Val]()

  def deriveObjectType[Ctx : WeakTypeTag, Val : WeakTypeTag](config: Tree*) = {
    val ctx = implicitly[WeakTypeTag[Ctx]]
    val v = implicitly[WeakTypeTag[Val]]

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

  def collectFields(config: Seq[MacroDeriveConfig], ctxType: Type, valType: Type): Either[List[(Position, String)], List[Tree]] = {
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

  def findKnownMembers(tpe: Type): List[KnownMember] =
    tpe.members.collect {
      case m: MethodSymbol if m.isCaseAccessor ⇒ KnownMember(tpe, m, findCaseClassAccessorAnnotations(tpe, m))
    }.toList.reverse

  def findCaseClassAccessorAnnotations(tpe: Type, member: MethodSymbol): List[Annotation] =
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

  def extractFields(knownMembers: List[KnownMember], config: Seq[MacroDeriveConfig]) = {
    val included = config.foldLeft(Set.empty[String]){
      case (acc, MacroIncludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, _) ⇒ acc
    }

    val excluded = config.foldLeft(Set.empty[String]){
      case (acc, MacroExcludeFields(fields, _)) ⇒ acc ++ fields
      case (acc, _) ⇒ acc
    }

    val actualIncluded =
      if (included.nonEmpty) included
      else knownMembers.map(m ⇒ m.name).toSet

    val actualFields = actualIncluded -- excluded

    knownMembers.filter(m ⇒ actualFields.contains(m.name) && !memberExcluded(m))
  }

  def validateFieldConfig(knownMembers: List[KnownMember], config: Seq[MacroDeriveConfig]) = {
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

      case _ ⇒ Nil
    }
  }

  def additionalFields(config: Seq[MacroDeriveConfig]) =
    config.foldLeft(List[Tree]()){
      case (acc, fields: MacroAddFields) ⇒ acc ++ fields.fields
      case (acc, _) ⇒ acc
    }

  def validateConfig(config: Seq[Tree]) = config.map {
    case q"Description.apply[$_, $_]($description)" ⇒
      Right(MacroDescription(description))

    case q"Name.apply[$_, $_]($name)" ⇒
      Right(MacroName(name))

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

    case tree ⇒ Left(tree.pos,
      "Unsupported shape of derivation config. " +
          "Please define subclasses of `DeriveConfig` directly in the argument list of the macro.")
  }

  def reportErrors(errors: Seq[(Position, String)]) = {
    require(errors.nonEmpty)

    val (lastPos, lastError) = errors.last

    errors.dropRight(1).foreach{case (pos, error) ⇒ c.error(pos, error)}

    c.abort(lastPos, lastError)
  }

  def symbolName(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLName] ⇒ arg}
      .headOption

  def symbolDescription(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDescription] ⇒ arg}
      .headOption

  def symbolDeprecation(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDeprecated] ⇒ arg}
      .headOption

  def symbolFieldTags(annotations: List[Annotation]): Tree =
    annotations
      .map (_.tree)
      .foldLeft(q"List[sangria.execution.FieldTag]()") {
        case (acc, q"new $name(..$fieldTags)") if name.tpe =:= typeOf[GraphQLFieldTags] ⇒
          q"$acc ++ $fieldTags"
        case (acc, _) ⇒ acc
      }

  def memberExcluded(member: KnownMember): Boolean =
    member.annotations.find(_.tree.tpe =:= typeOf[GraphQLExclude]).fold(false)(_ ⇒ true)

  case class KnownMember(onType: Type, method: MethodSymbol, annotations: List[Annotation]) {
    lazy val name = method.name.decodedName.toString
  }

  sealed trait MacroDeriveConfig

  case class MacroName(name: Tree) extends MacroDeriveConfig
  case class MacroDescription(description: Tree) extends MacroDeriveConfig
  case class MacroInterfaces(interfaces: Seq[Tree]) extends MacroDeriveConfig

  case class MacroDocumentField(fieldName: String, description: Tree, deprecationReason: Tree, pos: Position) extends MacroDeriveConfig
  case class MacroRenameField(fieldName: String, graphqlName: Tree, pos: Position) extends MacroDeriveConfig
  case class MacroFieldTags(fieldName: String, tags: Seq[Tree], pos: Position) extends MacroDeriveConfig
  case class MacroDeprecateField(fieldName: String, deprecationReason: Tree, pos: Position) extends MacroDeriveConfig

  case class MacroIncludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveConfig
  case class MacroExcludeFields(fieldNames: Set[String], pos: Position) extends MacroDeriveConfig
  case class MacroAddFields(fields: List[Tree]) extends MacroDeriveConfig
}