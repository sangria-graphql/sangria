package test

// these classes are in totally different package in order to ensure that macro generates correct identifiers that
// can be accessed outside of the `sangria` package.

import sangria.macros.derive._
import sangria.schema.{EnumType, ObjectType, OutputType}

case class CompanionA(b: CompanionB)
object CompanionA {
  implicit val graphqlType: ObjectType[Unit, CompanionA] = deriveObjectType[Unit, CompanionA]()
}

case class CompanionB(c: CompanionC)
object CompanionB {
  implicit val graphqlType: OutputType[CompanionB] =
    deriveObjectType[Unit, CompanionB](RenameField("c", "myC"))
}

case class CompanionC(e: CompanionEnum, e1: AnotherEnum.ValName)
object CompanionC {
  implicit def graphqlType[Ctx]: ObjectType[Ctx, CompanionC] = deriveObjectType[Ctx, CompanionC]()
}

sealed trait CompanionEnum

object CompanionEnum1 extends CompanionEnum
object CompanionEnum2 extends CompanionEnum

object CompanionEnum {
  implicit val graphqlType: EnumType[CompanionEnum] =
    deriveEnumType[CompanionEnum](RenameValue("CompanionEnum1", "first"))
}

object AnotherEnum extends Enumeration {
  type ValName = Value
  val FOO, BAR, BAZ = Value

  implicit val valNameType: EnumType[AnotherEnum.ValName] = deriveEnumType()
}
