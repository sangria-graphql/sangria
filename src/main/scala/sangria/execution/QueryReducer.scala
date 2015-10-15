package sangria.execution

import sangria.ast
import sangria.schema._

import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.Future
import scala.util.{Try, Failure, Success}

trait QueryReducer[-Ctx, +Out] {
  type Acc

  def initial: Acc

  def reduceAlternatives(alterntives: Seq[Acc]): Acc

  def reduceField[Val](
    fieldAcc: Acc,
    childrenAcc: Acc,
    path: List[String],
    ctx: Ctx,
    astFields: List[ast.Field],
    parentType: ObjectType[Out, Val] @uncheckedVariance,
    field: Field[Ctx, Val] @uncheckedVariance,
    argumentValuesFn: (List[String], List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc

  def reduceScalar[T](
    path: List[String],
    ctx: Ctx,
    tpe: ScalarType[T]): Acc

  def reduceEnum[T](
    path: List[String],
    ctx: Ctx,
    tpe: EnumType[T]): Acc

  def reduceCtx(acc: Acc, ctx: Ctx): ReduceAction[Out, Out]
}

object QueryReducer {
  def measureComplexity[Ctx](fn: (Double, Ctx) ⇒ ReduceAction[Ctx, Ctx]): QueryReducer[Ctx, Ctx] =
    new MeasureComplexity[Ctx](fn)

  def rejectComplexQueries[Ctx](complexityThreshold: Double, error: (Double, Ctx) ⇒ Throwable): QueryReducer[Ctx, Ctx] =
    new MeasureComplexity[Ctx]((c, ctx) ⇒
      if (c >= complexityThreshold) throw error(c, ctx) else ctx)

  def collectTags[Ctx, T](tagMatcher: PartialFunction[FieldTag, T])(fn: (Seq[T], Ctx) ⇒ ReduceAction[Ctx, Ctx]): QueryReducer[Ctx, Ctx] =
    new TagCollector[Ctx, T](tagMatcher, fn)
}

class MeasureComplexity[Ctx](action: (Double, Ctx) ⇒ ReduceAction[Ctx, Ctx]) extends QueryReducer[Ctx, Ctx] {
  type Acc = Double

  import MeasureComplexity.DefaultComplexity

  val initial = 0.0D

  def reduceAlternatives(alterntives: Seq[Acc]) = alterntives.max

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: List[String],
      ctx: Ctx,
      astFields: List[ast.Field],
      parentType: ObjectType[Ctx, Val],
      field: Field[Ctx, Val],
      argumentValuesFn: (List[String], List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc = {
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
    path: List[String],
    ctx: Ctx,
    tpe: ScalarType[T]): Acc = tpe.complexity

  def reduceEnum[T](
    path: List[String],
    ctx: Ctx,
    tpe: EnumType[T]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

object MeasureComplexity {
  val DefaultComplexity = 1.0D
}

class TagCollector[Ctx, T](tagMatcher: PartialFunction[FieldTag, T], action: (Seq[T], Ctx) ⇒ ReduceAction[Ctx, Ctx]) extends QueryReducer[Ctx, Ctx] {
  type Acc = Vector[T]

  val initial = Vector.empty

  def reduceAlternatives(alterntives: Seq[Acc]) = alterntives.toVector.flatten

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: List[String],
      ctx: Ctx,
      astFields: List[ast.Field],
      parentType: ObjectType[Ctx, Val],
      field: Field[Ctx, Val],
      argumentValuesFn: (List[String], List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc =
    fieldAcc ++ childrenAcc ++ field.tags.collect {case t if tagMatcher.isDefinedAt(t) ⇒ tagMatcher(t)}

  def reduceScalar[ST](
    path: List[String],
    ctx: Ctx,
    tpe: ScalarType[ST]): Acc = initial

  def reduceEnum[ET](
    path: List[String],
    ctx: Ctx,
    tpe: EnumType[ET]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}