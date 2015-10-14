package sangria.execution

import sangria.ast
import sangria.schema._

import scala.concurrent.Future
import scala.util.{Try, Failure, Success}

trait QueryReducer {
  type Acc

  def initial: Acc

  def reduceAlternatives(alterntives: Seq[Acc]): Acc

  def reduceField[Ctx, Val](
    fieldAcc: Acc,
    childrenAcc: Acc,
    path: List[String],
    ctx: Ctx,
    astFields: List[ast.Field],
    parentType: ObjectType[Ctx, Val],
    field: Field[Ctx, Val],
    argumentValuesFn: (List[String], List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc

  def reduceScalar[Ctx, T](
    path: List[String],
    ctx: Ctx,
    tpe: ScalarType[T]): Acc

  def reduceEnum[Ctx, T](
    path: List[String],
    ctx: Ctx,
    tpe: EnumType[T]): Acc

  def reduceCtx[Ctx](acc: Acc, ctx: Ctx): Either[Future[Ctx], Ctx]
}

object QueryReducer {
  def measureComplexity[Ctx](fn: (Double, Ctx) ⇒ Either[Future[Ctx], Ctx]): QueryReducer =
    new MeasureComplexity[Ctx](fn)

  def rejectComplexQueries(complexityThreshold: Double, error: Double ⇒ Throwable): QueryReducer =
    new MeasureComplexity[Any]((c, ctx) ⇒
      if (c >= complexityThreshold) throw error(c) else Right(ctx))

  def collectTags[Ctx, T](tagMatcher: PartialFunction[FieldTag, T])(fn: (Seq[T], Ctx) ⇒ Either[Future[Ctx], Ctx]): QueryReducer =
    new TagCollector[Ctx, T](tagMatcher, fn)
}

class MeasureComplexity[Ctx](action: (Double, Ctx) ⇒ Either[Future[Ctx], Ctx]) extends QueryReducer {
  type Acc = Double

  import MeasureComplexity.DefaultComplexity

  val initial = 0.0D

  def reduceAlternatives(alterntives: Seq[Acc]) = alterntives.max

  def reduceField[C, Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: List[String],
      ctx: C,
      astFields: List[ast.Field],
      parentType: ObjectType[C, Val],
      field: Field[C, Val],
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

  def reduceScalar[C, T](
    path: List[String],
    ctx: C,
    tpe: ScalarType[T]): Acc = tpe.complexity

  def reduceEnum[C, T](
    path: List[String],
    ctx: C,
    tpe: EnumType[T]): Acc = initial

  def reduceCtx[C](acc: Acc, ctx: C) =
    action(acc, ctx.asInstanceOf[Ctx]).asInstanceOf[Either[Future[C], C]]
}

object MeasureComplexity {
  val DefaultComplexity = 1.0D
}

class TagCollector[Ctx, T](tagMatcher: PartialFunction[FieldTag, T], action: (Seq[T], Ctx) ⇒ Either[Future[Ctx], Ctx]) extends QueryReducer {
  type Acc = Vector[T]

  val initial = Vector.empty

  def reduceAlternatives(alterntives: Seq[Acc]) = alterntives.toVector.flatten

  def reduceField[C, Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: List[String],
      ctx: C,
      astFields: List[ast.Field],
      parentType: ObjectType[C, Val],
      field: Field[C, Val],
      argumentValuesFn: (List[String], List[Argument[_]], List[ast.Argument]) ⇒ Try[Args]): Acc =
    fieldAcc ++ childrenAcc ++ field.tags.collect {case t if tagMatcher.isDefinedAt(t) ⇒ tagMatcher(t)}

  def reduceScalar[C, ST](
    path: List[String],
    ctx: C,
    tpe: ScalarType[ST]): Acc = initial

  def reduceEnum[C, ET](
    path: List[String],
    ctx: C,
    tpe: EnumType[ET]): Acc = initial

  def reduceCtx[C](acc: Acc, ctx: C) =
    action(acc, ctx.asInstanceOf[Ctx]).asInstanceOf[Either[Future[C], C]]
}