package sangria.macros

import sangria.schema.{EnumType, InputObjectType, ObjectType}
import scala.quoted._

package object derive {
  inline def deriveContextObjectType[Ctx, CtxVal, Val](
      inline fn: Ctx => CtxVal,
      inline config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Val] =
    ${ DeriveObjectTypeMacro.deriveContextObjectType[Ctx, CtxVal, Val]('fn, 'config) }

  inline def deriveObjectType[Ctx, Val](
      inline config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Val] =
    ${ DeriveObjectTypeMacro.deriveNormalObjectType[Ctx, Val]('config) }

  inline def deriveInputObjectType[T](
      inline config: DeriveInputObjectSetting*): InputObjectType[T] =
    ${ DeriveInputObjectTypeMacro.deriveInputObjectType[T]('config) }

  inline def deriveEnumType[T](inline config: DeriveEnumSetting*): EnumType[T] =
    ${ DeriveEnumTypeMacro.deriveEnumType[T]('config) }
}
