package sangria.execution

import sangria.ast
import sangria.schema._

import scala.annotation.unchecked.uncheckedVariance
import scala.util.{Failure, Success, Try}

trait QueryReducer[-Ctx, +Out] {
  type Acc

  def initial: Acc

  def reduceAlternatives(alternatives: Seq[Acc]): Acc

  def reduceField[Val](
    fieldAcc: Acc,
    childrenAcc: Acc,
    path: ExecutionPath,
    ctx: Ctx,
    astFields: Vector[ast.Field],
    parentType: ObjectType[Out, Val] @uncheckedVariance,
    field: Field[Ctx, Val] @uncheckedVariance,
    argumentValuesFn: (ExecutionPath, List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc

  def reduceScalar[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: ScalarType[T]): Acc

  def reduceEnum[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: EnumType[T]): Acc

  def reduceCtx(acc: Acc, ctx: Ctx): ReduceAction[Out, Out]
}

object QueryReducer {
  def measureComplexity[Ctx](fn: (Double, Ctx) ⇒ ReduceAction[Ctx, Ctx]): QueryReducer[Ctx, Ctx] =
    new MeasureComplexity[Ctx](fn)

  def rejectComplexQueries[Ctx](complexityThreshold: Double, error: (Double, Ctx) ⇒ Throwable): QueryReducer[Ctx, Ctx] =
    measureComplexity[Ctx]((c, ctx) ⇒
      if (c >= complexityThreshold) throw error(c, ctx) else ctx)

  def measureDepth[Ctx](fn: (Int, Ctx) ⇒ ReduceAction[Ctx, Ctx]): QueryReducer[Ctx, Ctx] =
    new MeasureQueryDepth[Ctx](fn)

  def rejectMaxDepth[Ctx](maxDepth: Int): QueryReducer[Ctx, Ctx] =
    measureDepth[Ctx]((depth, ctx) ⇒
      if (depth > maxDepth) throw new MaxQueryDepthReachedError(maxDepth) else ctx)

  def collectTags[Ctx, T](tagMatcher: PartialFunction[FieldTag, T])(fn: (Seq[T], Ctx) ⇒ ReduceAction[Ctx, Ctx]): QueryReducer[Ctx, Ctx] =
    new TagCollector[Ctx, T](tagMatcher, fn)
}

class MeasureComplexity[Ctx](action: (Double, Ctx) ⇒ ReduceAction[Ctx, Ctx]) extends QueryReducer[Ctx, Ctx] {
  type Acc = Double

  import MeasureComplexity.DefaultComplexity

  val initial = 0.0D

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.max

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val],
      field: Field[Ctx, Val],
      argumentValuesFn: (ExecutionPath, List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc = {
    val estimate = field.complexity match {
      case Some(fn) ⇒
        argumentValuesFn(path, field.arguments, astFields.head.arguments) match {
          case Success(args) ⇒ fn(ctx, args, childrenAcc)
          case Failure(_) ⇒ DefaultComplexity + childrenAcc
        }
      case None ⇒ DefaultComplexity + childrenAcc
    }

    fieldAcc + estimate
  }

  def reduceScalar[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: ScalarType[T]): Acc = tpe.complexity

  def reduceEnum[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: EnumType[T]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

class MeasureQueryDepth[Ctx](action: (Int, Ctx) ⇒ ReduceAction[Ctx, Ctx]) extends QueryReducer[Ctx, Ctx] {
  type Acc = Int

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.max

  def initial: Acc = 0

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val],
      field: Field[Ctx, Val],
      argumentValuesFn: (ExecutionPath, List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc =
    childrenAcc

  def reduceScalar[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: ScalarType[T]): Acc = path.size

  def reduceEnum[T](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: EnumType[T]): Acc = path.size

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

object MeasureComplexity {
  val DefaultComplexity = 1.0D
}

class TagCollector[Ctx, T](tagMatcher: PartialFunction[FieldTag, T], action: (Seq[T], Ctx) ⇒ ReduceAction[Ctx, Ctx]) extends QueryReducer[Ctx, Ctx] {
  type Acc = Vector[T]

  val initial = Vector.empty

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.toVector.flatten

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val],
      field: Field[Ctx, Val],
      argumentValuesFn: (ExecutionPath, List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc =
    fieldAcc ++ childrenAcc ++ field.tags.collect {case t if tagMatcher.isDefinedAt(t) ⇒ tagMatcher(t)}

  def reduceScalar[ST](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: ScalarType[ST]): Acc = initial

  def reduceEnum[ET](
    path: ExecutionPath,
    ctx: Ctx,
    tpe: EnumType[ET]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}