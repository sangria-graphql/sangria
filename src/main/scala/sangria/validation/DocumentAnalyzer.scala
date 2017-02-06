package sangria.validation

import sangria.ast
import sangria.ast.AstVisitor
import sangria.ast.AstVisitorCommand.{Continue, Skip}
import sangria.schema._
import sangria.introspection.isIntrospection

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{ListBuffer, Set ⇒ MutableSet, Map ⇒ MutableMap}

case class DocumentAnalyzer(schema: Schema[_, _], document: ast.Document) {
  import DocumentAnalyzer._

  private val fragmentSpreadsCache = TrieMap[Int, List[ast.FragmentSpread]]()
  private val recursivelyReferencedFragmentsCache = TrieMap[Int, List[ast.FragmentDefinition]]()
  private val variableUsages = TrieMap[Int, List[VariableUsage]]()
  private val recursiveVariableUsages = TrieMap[Int, List[VariableUsage]]()

  def getFragmentSpreads(astNode: ast.SelectionContainer) =
    fragmentSpreadsCache.getOrElseUpdate(astNode.cacheKeyHash, {
      val spreads = ListBuffer[ast.FragmentSpread]()
      val setsToVisit = ValidatorStack.empty[List[ast.Selection]]

      setsToVisit.push(astNode.selections)

      while (setsToVisit.nonEmpty) {
        val set = setsToVisit.pop()

        set.foreach {
          case fs: ast.FragmentSpread ⇒
            spreads += fs
          case cont: ast.SelectionContainer ⇒
            setsToVisit push cont.selections
        }
      }

      spreads.toList
    })

  def getRecursivelyReferencedFragments(operation: ast.OperationDefinition) =
    recursivelyReferencedFragmentsCache.getOrElseUpdate(operation.cacheKeyHash, {
      val frags = ListBuffer[ast.FragmentDefinition]()
      val collectedNames = MutableSet[String]()
      val nodesToVisit = ValidatorStack.empty[ast.SelectionContainer]

      nodesToVisit.push(operation)

      while (nodesToVisit.nonEmpty) {
        val node = nodesToVisit.pop()
        val spreads = getFragmentSpreads(node)

        spreads.foreach { spread ⇒
          val fragName = spread.name

          if (!collectedNames.contains(fragName)) {
            collectedNames += fragName

            document.fragments.get(fragName) match {
              case Some(frag) ⇒
                frags += frag
                nodesToVisit.push(frag)
              case None ⇒ // do nothing
            }
          }
        }
      }

      frags.toList
    })

  def getVariableUsages(astNode: ast.SelectionContainer) =
    variableUsages.getOrElseUpdate(astNode.cacheKeyHash, {
      AstVisitor.visitAstWithState(schema, astNode, ListBuffer[VariableUsage]()) { (typeInfo, usages) ⇒
        AstVisitor {
          case _: ast.VariableDefinition ⇒ Skip
          case vv: ast.VariableValue ⇒
            usages += VariableUsage(vv, typeInfo.inputType)
            Continue
        }
      }.toList
    })

  def getRecursiveVariableUsages(operation: ast.OperationDefinition) =
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
}

object DocumentAnalyzer {
  case class VariableUsage(node: ast.VariableValue, tpe: Option[InputType[_]])

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
