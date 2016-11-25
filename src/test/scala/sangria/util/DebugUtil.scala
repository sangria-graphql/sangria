package sangria.util

import org.scalactic.Prettifier

import scala.collection.GenMap
import org.scalactic.PrettyMethods.Prettyizer
import sangria.ast._

object DebugUtil {

  private val indentClasses: PartialFunction[Any, Boolean] = {
    case v if v.getClass.getSimpleName.startsWith("Introspection") ⇒ true
    case _: Document |
         _: Definition |
         _: SelectionContainer |
         _: Directive |
         _: VariableDefinition |
         _: ObjectValue |
         _: ObjectField |
         _: ListValue |
         _: Argument ⇒ true
  }

  private val myPrettifier: Prettifier =
    new Prettifier {
      def apply(o: Any): String = {
        def indent(n: Int) = "  " * n

        def loop(obj: Any, level: Int, indentLists: Boolean = false, indentMap: Boolean = false): String =
          obj match {
            case null ⇒ "null"
            case aString: String ⇒ "\"" + StringUtil.escapeString(aString) + "\""
            case aStringWrapper: scala.collection.immutable.StringOps ⇒ "\"" + aStringWrapper + "\""
            case aChar: Char ⇒  "\'" + aChar + "\'"
            case ot: OperationType ⇒ "OperationType." + ot
            case aGenMap: GenMap[_, _] ⇒
              (if (indentMap) indent(level + 1) else "") + "Map(\n" +
                aGenMap.toIterator.map { case (key, value) ⇒
                  indent(level + 1) + loop(key, level) + " → " + loop(value, level + 1, indentMap = false, indentLists = true)
                }.mkString(",\n") + ")"
            case list: scala.collection.immutable.List[_] ⇒
              if (list.isEmpty) "Nil"
              else
                if (indentLists)
                  "List(\n" + list.map(x ⇒ indent(level + 1) + loop(x, level + 1)).mkString(",\n") + ")"
                else
                  "List(" + list.map(x ⇒ loop(x, level)).mkString(", ") + ")"
            case list: scala.collection.immutable.Vector[_] ⇒
              if (list.isEmpty) "Vector.empty"
              else
                if (indentLists)
                  "Vector(\n" + list.map(x ⇒ indent(level + 1) + loop(x, level + 1)).mkString(",\n") + ")"
                else
                  "Vector(" + list.map(x ⇒ loop(x, level)).mkString(", ") + ")"
            case prod: Product ⇒
              val args = prod.productIterator.toList

              if (args.nonEmpty)
                if (indentClasses.isDefinedAt(prod) && indentClasses(prod))
                  prod.productPrefix + "(\n" + args.map(x ⇒ indent(level + 1) + loop(x, level + 1, true)).mkString(",\n") + "\n" + indent(level) + ")"
                else
                  prod.productPrefix + "(" + args.map(x ⇒ loop(x, level, false)).mkString(", ") + ")"
              else
                prod.productPrefix
            case anythingElse ⇒
              anythingElse.toString
          }

        loop(o, 0, false)
      }
    }

  private implicit val conf = myPrettifier

  def prettyRender(obj: Any) = obj.pretty

  def prettyPrint(obj: Any) = println(prettyRender(obj))

}
