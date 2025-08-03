package sangria.macros.derive

import sangria.schema.{InputType, OutputType}

import scala.quoted._
import scala.reflect.ClassTag

private[derive] trait DeriveMacroSupport {

  def reportErrors(using Quotes)(errors: Seq[(PositionPointer, String)]) = {
    import quotes.reflect.report
    val (lastPos, lastError) = errors.last

    errors.dropRight(1).foreach { case (pos, error) =>
      pos match {
        case PositionByExpr(expr) =>
          report.error(error, expr)
        case PositionByQuotes(q) =>
          q.reflect.report.error(error, q.reflect.Position.ofMacroExpansion)
      }
    }

    lastPos match {
      case PositionByExpr(expr) =>
        report.errorAndAbort(lastError, expr)
      case PositionByQuotes(q) =>
        q.reflect.report.errorAndAbort(lastError, q.reflect.Position.ofMacroExpansion)
    }
  }

  def reportSummoningErrors(errorStrings: Seq[String], summonOptions: Seq[Option[_]])(using
      Quotes) = {
    val errorList = summonOptions.toList.zip(errorStrings).collect { case (None, error) =>
      error
    }
    errorList.tail.foreach(quotes.reflect.report.error(_))
    quotes.reflect.report.errorAndAbort(errorList.head)
  }

  protected def symbolName(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Option[Expr[String]] =
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLName($arg) } => arg }
      .headOption

  protected def symbolOutputType(using Quotes)(
      annotations: List[quotes.reflect.Term]): Option[Expr[OutputType[_]]] =
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLOutputType($arg) } => arg }
      .headOption

  protected def symbolInputType(using Quotes)(
      annotations: List[quotes.reflect.Term]): Option[Expr[InputType[_]]] =
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLInputType($arg) } => arg }
      .headOption

  protected def symbolDescription(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Option[Expr[String]] =
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLDescription($arg) } => arg }
      .headOption

  protected def symbolDefault(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Option[(quotes.reflect.TypeRepr, Expr[Any])] =
    import quotes.reflect._
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLDefault[t]($arg) } => (TypeRepr.of[t], arg) }
      .headOption

  protected def symbolDeprecation(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Option[Expr[String]] =
    annotations
      .map(_.asExpr)
      .collect { case '{ new GraphQLDeprecated($arg) } => arg }
      .headOption

  protected def symbolFieldTags(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Expr[List[sangria.execution.FieldTag]] =
    import quotes.reflect._
    annotations
      .map(_.asExpr)
      .foldLeft('{ List[sangria.execution.FieldTag]() }) {
        case (acc, '{ new GraphQLFieldTags(${ Varargs(fieldTags) }: _*) }) =>
          '{ $acc ++ ${ Expr.ofList(fieldTags) } }
        case (acc, _) => acc
      }

  protected def memberExcluded(using quotes: Quotes)(
      annotations: List[quotes.reflect.Term]): Boolean =
    import quotes.reflect._
    annotations.find(_.tpe =:= TypeRepr.of[GraphQLExclude]).fold(false)(_ => true)

  protected def memberField(using Quotes)(annotations: List[quotes.reflect.Term]): Boolean =
    import quotes.reflect._
    annotations.find(_.tpe =:= TypeRepr.of[GraphQLField]).fold(false)(_ => true)

  protected def flattenOptionExpr[T](using Quotes, Type[T])(
      exprOpt: Option[Expr[T]]): Expr[Option[T]] = {
    import quotes.reflect._
    exprOpt match {
      case Some(expr) => '{ Some($expr) }
      case None => '{ Option.empty[T] }
    }
  }

  protected def unsafeSelectByName[T: Type](using quotes: Quotes)(
      memberExpr: Expr[_],
      name: String
  ): Expr[T] = {
    import quotes.reflect._
    Select.unique(memberExpr.asTerm, name).asExprOf[T]
  }

  protected def getClassTag[T](using Type[T], Quotes): Expr[ClassTag[T]] = {
    import quotes.reflect._

    Expr.summon[ClassTag[T]] match {
      case Some(ct) =>
        ct
      case None =>
        report.errorAndAbort(
          s"Unable to find a ClassTag for type ${Type.show[T]}",
          Position.ofMacroExpansion
        )
    }

  }
}
