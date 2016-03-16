package test

import sangria.macros.derive._
import sangria.schema.{OutputType, ObjectType}

case class CompanionA(b: CompanionB)
object CompanionA {
  implicit val graphqlType: ObjectType[Unit, CompanionA] = deriveObjectType[Unit, CompanionA]()
}

case class CompanionB(c: CompanionC)
object CompanionB {
  implicit val graphqlType: OutputType[CompanionB] = deriveObjectType[Unit, CompanionB](
    RenameField("c", "myC"))
}

case class CompanionC(e: CompanionEnum)
object CompanionC {
  implicit def graphqlType[Ctx]: ObjectType[Ctx, CompanionC] = deriveObjectType[Ctx, CompanionC]()
}

sealed trait CompanionEnum

object CompanionEnum1 extends CompanionEnum
object CompanionEnum2 extends CompanionEnum

object CompanionEnum {
  implicit val graphqlType: OutputType[CompanionEnum] = deriveEnumType[CompanionEnum](
    RenameValue("CompanionEnum1", "first"))
}
