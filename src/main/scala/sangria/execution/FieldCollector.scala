package sangria.execution

import sangria.ast.OperationType
import sangria.parser.SourceMapper
import sangria.schema._
import sangria.ast

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap
import scala.collection.mutable.{Set ⇒ MutableSet}

import scala.util.{Try, Failure, Success}

class FieldCollector[Ctx, Val](
    schema: Schema[Ctx, Val],
    document: ast.Document,
    variables: Map[String, VariableValue],
    sourceMapper: Option[SourceMapper],
    valueCollector: ValueCollector[Ctx, _],
    exceptionHandler: Executor.ExceptionHandler) {

  private val resultCache = TrieMap[(Vector[String], String), Try[Map[String, (ast.Field, Try[Vector[ast.Field]])]]]()

  def collectFields(path: Vector[String], tpe: ObjectType[Ctx, _], selections: Vector[ast.SelectionContainer]): Try[Map[String, (ast.Field, Try[Vector[ast.Field]])]] =
    resultCache.getOrElseUpdate(path → tpe.name,
      selections.foldLeft(Success(ListMap.empty): Try[Map[String, (ast.Field, Try[Vector[ast.Field]])]]) {
        case (acc, s) ⇒ collectFieldsInternal(tpe, s.selections, MutableSet.empty, acc)
      })

  private def collectFieldsInternal(tpe: ObjectType[Ctx, _], selections: List[ast.Selection], visitedFragments: MutableSet[String], initial: Try[Map[String, (ast.Field, Try[Vector[ast.Field]])]]): Try[Map[String, (ast.Field, Try[Vector[ast.Field]])]] =
    selections.foldLeft(initial) {
      case (f @ Failure(_), selection) ⇒ f
      case (s @ Success(acc), selection) ⇒
        selection match {
          case field @ ast.Field(_, _, _, dirs, _, _) ⇒
            val name = field.outputName

            shouldIncludeNode(dirs, selection) match {
              case Success(true) ⇒ acc.get(name) match {
                case Some((f, Success(list))) ⇒ Success(acc.updated(name, f → Success(list :+ field)))
                case Some((_, Failure(_))) ⇒ s
                case None ⇒ Success(acc.updated(name, field → Success(Vector(field))))
              }
              case Success(false) ⇒ s
              case Failure(error) ⇒ Success(acc.updated(name, field → Failure(error)))
            }
          case fragment @ ast.InlineFragment(_, dirs, fragmentSelections, _) ⇒
            for {
              shouldInclude ← shouldIncludeNode(dirs, selection)
              fragmentConditionMatch ← doesFragmentConditionMatch(tpe, fragment)
              fragmentFields ←
                if (shouldInclude && fragmentConditionMatch)
                  collectFieldsInternal(tpe, fragmentSelections, visitedFragments, s)
                else s
            } yield fragmentFields
          case ast.FragmentSpread(name, _, _) if visitedFragments contains name ⇒ s
          case ast.FragmentSpread(name, dirs, position) ⇒
            shouldIncludeNode(dirs, selection) flatMap { shouldInclude ⇒

              if (shouldInclude) {
                visitedFragments += name

                document.fragments.get(name) match {
                  case Some(fragment) ⇒
                    for {
                      shouldInclude ← shouldIncludeNode(fragment.directives, fragment)
                      fragmentConditionMatch ← doesFragmentConditionMatch(tpe, fragment)
                      fragmentFields ←
                        if (shouldInclude && fragmentConditionMatch)
                          collectFieldsInternal(tpe, fragment.selections, visitedFragments, s)
                        else s
                    } yield fragmentFields
                  case None ⇒
                    Failure(new ExecutionError(s"Fragment with name '$name' is not defined", exceptionHandler, sourceMapper, position.toList))
                }
              } else s
            }
        }
    }

  def shouldIncludeNode(directives: List[ast.Directive], selection: ast.WithDirectives): Try[Boolean] = {
    val possibleDirs = directives
        .map(d ⇒ schema.directivesByName
          .get(d.name)
          .map(dd ⇒ selection match {
            case _: ast.Field if !dd.locations.contains(DirectiveLocation.Field) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on fields", exceptionHandler, sourceMapper, d.position.toList))
            case _: ast.InlineFragment if !dd.locations.contains(DirectiveLocation.InlineFragment) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on inline fragment", exceptionHandler, sourceMapper, d.position.toList))
            case _: ast.FragmentSpread if !dd.locations.contains(DirectiveLocation.FragmentSpread) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on fragment spread", exceptionHandler, sourceMapper, d.position.toList))
            case _: ast.FragmentDefinition if !dd.locations.contains(DirectiveLocation.FragmentDefinition) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on fragment definition", exceptionHandler, sourceMapper, d.position.toList))
            case op: ast.OperationDefinition if op.operationType == OperationType.Query && !dd.locations.contains(DirectiveLocation.Query) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on query operation", exceptionHandler, sourceMapper, d.position.toList))
            case op: ast.OperationDefinition if op.operationType == OperationType.Mutation && !dd.locations.contains(DirectiveLocation.Mutation) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on mutation operation", exceptionHandler, sourceMapper, d.position.toList))
            case op: ast.OperationDefinition if op.operationType == OperationType.Subscription && !dd.locations.contains(DirectiveLocation.Subscription) ⇒
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on subscription operation", exceptionHandler, sourceMapper, d.position.toList))
            case _ ⇒ Success(d → dd)
          })
          .getOrElse(Failure(new ExecutionError(s"Directive '${d.name}' not found.", exceptionHandler, sourceMapper, d.position.toList))))
        .map(_.flatMap{case (astDir, dir) ⇒ valueCollector.getArgumentValues(dir.arguments, astDir.arguments, variables) map (dir → _)})

    possibleDirs.collect{case Failure(error) ⇒ error}.headOption map (Failure(_)) getOrElse {
      val validDirs = possibleDirs collect {case Success(v) ⇒ v}
      val should = validDirs.forall { case (dir, args) ⇒ dir.shouldInclude(DirectiveContext(selection, dir, args)) }

      Success(should)
    }
  }

  def doesFragmentConditionMatch(tpe: ObjectType[_, _], conditional: ast.ConditionalFragment): Try[Boolean] =
    conditional.typeConditionOpt match {
      case Some(tc) ⇒
        schema.outputTypes.get(tc.name)
          .map(condTpe ⇒ Success(condTpe.name == tpe.name || (condTpe.isInstanceOf[AbstractType] && schema.isPossibleType(condTpe.name, tpe))))
          .getOrElse(Failure(new ExecutionError(s"Unknown type '${tc.name}'.", exceptionHandler, sourceMapper, conditional.position.toList)))
      case None ⇒ Success(true)
    }
}
