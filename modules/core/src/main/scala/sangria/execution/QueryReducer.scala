package sangria.execution

import sangria.ast
import sangria.introspection.{TypeNameMetaField, isIntrospection}
import sangria.schema._
import scala.annotation.unchecked.uncheckedVariance
import scala.util.{Failure, Success, Try}

trait QueryReducer[-Ctx, +Out, F[_]] {
  type Acc

  def initial: Acc

  def reduceAlternatives(alternatives: Seq[Acc]): Acc

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Out, Val, F] @uncheckedVariance,
      field: Field[Ctx, Val, F] @uncheckedVariance,
      argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc

  def reduceScalar[T](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[T]): Acc

  def reduceEnum[T](path: ExecutionPath, ctx: Ctx, tpe: EnumType[T]): Acc

  def reduceCtx(acc: Acc, ctx: Ctx): ReduceAction[Out, Out, F]
}

object QueryReducer {
  type ArgumentValuesFn = (ExecutionPath, List[Argument[_]], Vector[ast.Argument]) => Try[Args]

  def measureComplexity[Ctx, F[_]](
      fn: (Double, Ctx) => ReduceAction[Ctx, Ctx, F]): QueryReducer[Ctx, Ctx, F] =
    new MeasureComplexity[Ctx, F](fn)

  def rejectComplexQueries[Ctx, F[_]](
      complexityThreshold: Double,
      error: (Double, Ctx) => Throwable): QueryReducer[Ctx, Ctx, F] =
    measureComplexity[Ctx, F]((c, ctx) =>
      if (c >= complexityThreshold) throw error(c, ctx) else ctx)

  def measureDepth[Ctx, F[_]](
      fn: (Int, Ctx) => ReduceAction[Ctx, Ctx, F]): QueryReducer[Ctx, Ctx, F] =
    new MeasureQueryDepth[Ctx, F](fn)

  def rejectMaxDepth[Ctx, F[_]](maxDepth: Int): QueryReducer[Ctx, Ctx, F] =
    measureDepth[Ctx, F]((depth, ctx) =>
      if (depth > maxDepth) throw new MaxQueryDepthReachedError(maxDepth) else ctx)

  def collectTags[Ctx, T, F[_]](tagMatcher: PartialFunction[FieldTag, T])(
      fn: (Seq[T], Ctx) => ReduceAction[Ctx, Ctx, F]): QueryReducer[Ctx, Ctx, F] =
    new TagCollector[Ctx, T, F](tagMatcher, fn)

  def rejectIntrospection[Ctx, F[_]](includeTypeName: Boolean = true): QueryReducer[Ctx, Ctx, F] =
    hasIntrospection(
      (hasIntro, ctx) => if (hasIntro) throw IntrospectionNotAllowedError else ctx,
      includeTypeName)

  def hasIntrospection[Ctx, F[_]](
      fn: (Boolean, Ctx) => ReduceAction[Ctx, Ctx, F],
      includeTypeName: Boolean = true): QueryReducer[Ctx, Ctx, F] =
    new HasIntrospectionReducer[Ctx, F](includeTypeName, fn)

}

class MeasureComplexity[Ctx, F[_]](action: (Double, Ctx) => ReduceAction[Ctx, Ctx, F])
    extends QueryReducer[Ctx, Ctx, F] {
  type Acc = Double

  import MeasureComplexity.DefaultComplexity

  val initial = 0.0d

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.max

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val, F],
      field: Field[Ctx, Val, F],
      argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc = {
    val estimate = field.complexity match {
      case Some(fn) =>
        argumentValuesFn(path, field.arguments, astFields.head.arguments) match {
          case Success(args) => fn(ctx, args, childrenAcc)
          case Failure(_) => DefaultComplexity + childrenAcc
        }
      case None => DefaultComplexity + childrenAcc
    }

    fieldAcc + estimate
  }

  def reduceScalar[T](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[T]): Acc = tpe.complexity

  def reduceEnum[T](path: ExecutionPath, ctx: Ctx, tpe: EnumType[T]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

class MeasureQueryDepth[Ctx, F[_]](action: (Int, Ctx) => ReduceAction[Ctx, Ctx, F])
    extends QueryReducer[Ctx, Ctx, F] {
  type Acc = Int

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.max

  def initial: Acc = 0

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val, F],
      field: Field[Ctx, Val, F],
      argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc =
    Math.max(fieldAcc, childrenAcc)

  def reduceScalar[T](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[T]): Acc = path.size

  def reduceEnum[T](path: ExecutionPath, ctx: Ctx, tpe: EnumType[T]): Acc = path.size

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

object MeasureComplexity {
  val DefaultComplexity = 1.0d
}

class TagCollector[Ctx, T, F[_]](
    tagMatcher: PartialFunction[FieldTag, T],
    action: (Seq[T], Ctx) => ReduceAction[Ctx, Ctx, F])
    extends QueryReducer[Ctx, Ctx, F] {
  type Acc = Vector[T]

  val initial = Vector.empty

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.toVector.flatten

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val, F],
      field: Field[Ctx, Val, F],
      argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc =
    fieldAcc ++ childrenAcc ++ field.tags.collect {
      case t if tagMatcher.isDefinedAt(t) => tagMatcher(t)
    }

  def reduceScalar[ST](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[ST]): Acc = initial

  def reduceEnum[ET](path: ExecutionPath, ctx: Ctx, tpe: EnumType[ET]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}

class HasIntrospectionReducer[Ctx, F[_]](
    includeTypeName: Boolean,
    action: (Boolean, Ctx) => ReduceAction[Ctx, Ctx, F])
    extends QueryReducer[Ctx, Ctx, F] {
  type Acc = Boolean

  val initial = false

  def reduceAlternatives(alternatives: Seq[Acc]) = alternatives.exists(hasIntro => hasIntro)

  def reduceField[Val](
      fieldAcc: Acc,
      childrenAcc: Acc,
      path: ExecutionPath,
      ctx: Ctx,
      astFields: Vector[ast.Field],
      parentType: ObjectType[Ctx, Val, F],
      field: Field[Ctx, Val, F],
      argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc = {
    val self =
      if (!includeTypeName && field.name == TypeNameMetaField.name) false
      else isIntrospection(parentType, field)

    fieldAcc || childrenAcc || self
  }

  def reduceScalar[ST](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[ST]): Acc = initial

  def reduceEnum[ET](path: ExecutionPath, ctx: Ctx, tpe: EnumType[ET]): Acc = initial

  def reduceCtx(acc: Acc, ctx: Ctx) =
    action(acc, ctx)
}
