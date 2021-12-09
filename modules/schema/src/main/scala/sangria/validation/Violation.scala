package sangria.validation

trait Violation {
  def errorMessage: String
}

abstract class BaseViolation(val errorMessage: String) extends Violation
abstract class ValueCoercionViolation(errorMessage: String) extends BaseViolation(errorMessage)

case class EnumValueCoercionViolation(name: String, typeName: String, knownValues: Seq[String])
  extends ValueCoercionViolation(
    s"Enum value '$name' is undefined in enum type '$typeName'. Known values are: ${knownValues
      .mkString(", ")}.")
case object EnumCoercionViolation extends ValueCoercionViolation(s"Enum value expected")

case class ConflictingTypeDefinitionViolation(
  typeName: String,
  conflictingTypes: List[String],
  parentInfo: String)
  extends Violation {
  lazy val errorMessage =
    s"Type name '$typeName' is used for several conflicting GraphQL type kinds: ${conflictingTypes
      .mkString(", ")}. Conflict found in $parentInfo."
}

case class ConflictingObjectTypeCaseClassViolation(typeName: String, parentInfo: String)
  extends Violation {
  // Ideally this error message should include the conflicting classes canonical names but due to https://issues.scala-lang.org/browse/SI-2034 that's not possible
  lazy val errorMessage =
    s"""Type name '$typeName' is used for several conflicting GraphQL ObjectTypes based on different classes. Conflict found in $parentInfo. One possible fix is to use ObjectTypeName like this: deriveObjectType[Foo, Bar](ObjectTypeName("OtherBar")) to avoid that two ObjectTypes have the same name."""
}

case class ConflictingInputObjectTypeCaseClassViolation(typeName: String, parentInfo: String)
  extends Violation {
  // Ideally this error message should include the conflicting classes canonical names but due to https://issues.scala-lang.org/browse/SI-2034 that's not possible
  lazy val errorMessage =
    s"""Type name '$typeName' is used for several conflicting GraphQL InputObjectTypes based on different classes. Conflict found in $parentInfo. One possible fix is to use InputObjectTypeName like this: deriveInputObjectType[Foo, Bar](InputObjectTypeName("OtherBar")) to avoid that two InputObjectTypes have the same name."""
}
