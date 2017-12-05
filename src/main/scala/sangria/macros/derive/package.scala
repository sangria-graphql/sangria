package sangria.macros

import scala.language.experimental.{macros => `scalac, please just let me do it!`}
import scala.language.higherKinds

import sangria.schema.{EnumType, InputObjectType, ObjectType}

package object derive {
  def deriveWrappedObjectType[Ctx, Wrapper[_] : ObjectWrapper, Val](config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Wrapper[Val]] =
    macro DeriveObjectTypeMacro.deriveWrappedObjectType[Ctx, Wrapper[_], Val]

  def deriveContextObjectType[Ctx, CtxVal, Val](fn: Ctx ⇒ CtxVal, config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Val] =
    macro DeriveObjectTypeMacro.deriveContextObjectType[Ctx, CtxVal, Val]

  def deriveObjectType[Ctx, Val](config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Val] =
    macro DeriveObjectTypeMacro.deriveNormalObjectType[Ctx, Val]

  def deriveInputObjectType[T](config: DeriveInputObjectSetting*): InputObjectType[T] =
    macro DeriveInputObjectTypeMacro.deriveInputObjectType[T]

  def deriveEnumType[T](config: DeriveEnumSetting*): EnumType[T] =
    macro DeriveEnumTypeMacro.deriveEnumType[T]
}
