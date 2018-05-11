package sangria.validation

import language.existentials
import sangria.ast
import sangria.ast.AstVisitor
import sangria.visitor.VisitorCommand.{Continue, Skip}
import sangria.schema._
import sangria.introspection.isIntrospection
import sangria.marshalling.ToInput
import sangria.util.Cache

import scala.collection.mutable.{ListBuffer, Map => MutableMap}

case class SchemaBasedDocumentAnalyzer(schema: Schema[_, _], document: ast.Document) {
  import SchemaBasedDocumentAnalyzer._

  private val variableUsages = Cache.empty[Int, List[VariableUsage]]
  private val recursiveVariableUsages = Cache.empty[Int, List[VariableUsage]]

  val documentAnalyzer = DocumentAnalyzer(document)

  def getVariableUsages(astNode: ast.SelectionContainer): List[VariableUsage] =
    variableUsages.getOrElseUpdate(astNode.cacheKeyHash, {
      AstVisitor.visitAstWithState(schema, astNode, ListBuffer[VariableUsage]()) { (typeInfo, usages) ⇒
        AstVisitor {
          case _: ast.VariableDefinition ⇒ Skip
          case vv: ast.VariableValue ⇒
            usages += VariableUsage(vv, typeInfo.inputType, typeInfo.defaultValue)
            Continue
        }
      }.toList
    })

  def getRecursiveVariableUsages(operation: ast.OperationDefinition): List[VariableUsage] =
    recursiveVariableUsages.getOrElseUpdate(operation.cacheKeyHash,
      getRecursivelyReferencedFragments(operation).foldLeft(getVariableUsages(operation)) {
        case (acc, fragment) ⇒ acc ++ getVariableUsages(fragment)
      })

  lazy val deprecatedUsages: Vector[DeprecatedUsage] =
    AstVisitor.visitAstWithState(schema, document, MutableMap[String, DeprecatedUsage]()) { (typeInfo, deprecated) ⇒
      AstVisitor.simple {
        case astField: ast.Field if typeInfo.fieldDef.isDefined && typeInfo.fieldDef.get.deprecationReason.isDefined && typeInfo.previousParentType.isDefined ⇒
          val parent = typeInfo.previousParentType.get
          val field = typeInfo.fieldDef.get

          val key = parent.name + "." + field.name

          if (!deprecated.contains(key))
            deprecated(key) = DeprecatedField(parent, field, astField, typeInfo.fieldDef.get.deprecationReason.get)

        case enumValue: ast.EnumValue ⇒
          typeInfo.inputType.map(_.namedType) match {
            case Some(parent: EnumType[_]) if typeInfo.enumValue.isDefined ⇒
              val value = typeInfo.enumValue.get
              val key = parent.name + "." + value.name

              if (value.deprecationReason.isDefined && !deprecated.contains(key))
                deprecated(key) = DeprecatedEnumValue(parent, value, enumValue, value.deprecationReason.get)

            case _ ⇒ // do nothing
          }

      }
    }.values.toVector

  lazy val introspectionUsages: Vector[IntrospectionUsage] =
    AstVisitor.visitAstWithState(schema, document, MutableMap[String, IntrospectionUsage]()) { (typeInfo, usages) ⇒
      AstVisitor.simple {
        case astField: ast.Field if typeInfo.fieldDef.isDefined && typeInfo.previousParentType.isDefined ⇒
          val parent = typeInfo.previousParentType.get
          val field = typeInfo.fieldDef.get

          if (isIntrospection(parent, field)) {
            val key = parent.name + "." + field.name

            if (!usages.contains(key))
              usages(key) = IntrospectionUsage(parent, field, astField)
          }
      }
    }.values.toVector

  def getFragmentSpreads(astNode: ast.SelectionContainer) = documentAnalyzer.getFragmentSpreads(astNode)
  def getRecursivelyReferencedFragments(operation: ast.OperationDefinition) = documentAnalyzer.getRecursivelyReferencedFragments(operation)
}

object SchemaBasedDocumentAnalyzer {
  case class VariableUsage(node: ast.VariableValue, tpe: Option[InputType[_]], defaultValue: Option[(_, ToInput[_, _])])

  sealed trait DeprecatedUsage extends Violation

  case class DeprecatedField(parentType: CompositeType[_], field: Field[_, _], astField: ast.Field, deprecationReason: String) extends DeprecatedUsage {
    def errorMessage = s"The field '${parentType.name}.${field.name}' is deprecated. $deprecationReason"
  }

  case class DeprecatedEnumValue(parentType: EnumType[_], value: EnumValue[_], astValue: ast.EnumValue, deprecationReason: String) extends DeprecatedUsage {
    def errorMessage = s"The enum value '${parentType.name}.${value.name}' is deprecated. $deprecationReason"
  }

  case class IntrospectionUsage(parentType: CompositeType[_], field: Field[_, _], astField: ast.Field) extends Violation {
    def errorMessage = s"Introspection field '${parentType.name}.${field.name}' is used."
  }
}
