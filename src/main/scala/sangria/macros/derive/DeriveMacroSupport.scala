package sangria.macros.derive

import scala.reflect.internal.{StdNames, SymbolTable, Definitions}
import scala.reflect.macros.blackbox

trait DeriveMacroSupport {
  val c: blackbox.Context
  val universe: c.universe.type = c.universe

  import c.universe._

  def reportErrors(errors: Seq[(Position, String)]) = {
    require(errors.nonEmpty)

    val (lastPos, lastError) = errors.last

    errors.dropRight(1).foreach{case (pos, error) ⇒ c.error(pos, error)}

    c.abort(lastPos, lastError)
  }

  protected def symbolName(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLName] ⇒ arg}
      .headOption

  protected def symbolDescription(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDescription] ⇒ arg}
      .headOption

  protected def symbolDefault(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDefault] ⇒ arg}
      .headOption

  protected def symbolDeprecation(annotations: List[Annotation]): Option[Tree] =
    annotations
      .map (_.tree)
      .collect {case q"new $name($arg)" if name.tpe =:= typeOf[GraphQLDeprecated] ⇒ arg}
      .headOption

  protected def symbolFieldTags(annotations: List[Annotation]): Tree =
    annotations
      .map (_.tree)
      .foldLeft(q"List[sangria.execution.FieldTag]()") {
        case (acc, q"new $name(..$fieldTags)") if name.tpe =:= typeOf[GraphQLFieldTags] ⇒
          q"$acc ++ $fieldTags"
        case (acc, _) ⇒ acc
      }

  protected def memberExcluded(annotations: List[Annotation]): Boolean =
    annotations.find(_.tree.tpe =:= typeOf[GraphQLExclude]).fold(false)(_ ⇒ true)

  protected def memberField(annotations: List[Annotation]): Boolean =
    annotations.find(_.tree.tpe =:= typeOf[GraphQLField]).fold(false)(_ ⇒ true)

  // TODO: most probably not needed, so should be removed in future
  protected def defaultMethodArgValue(method: String, pos: Int) = {
    val defs = c.universe.asInstanceOf[Definitions with SymbolTable with StdNames]

    defs.nme.defaultGetterName(defs.newTermName(method), pos)
  }

  def checkSetting[T : WeakTypeTag](setting: Tree) = weakTypeTag[T].tpe =:= c.typecheck(setting).tpe
}
