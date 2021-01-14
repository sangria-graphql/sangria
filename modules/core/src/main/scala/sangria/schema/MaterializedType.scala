package sangria.schema

import sangria.ast

sealed trait MaterializedType {
  def origin: MatOrigin
  def name: String
  def rename(newName: String): MaterializedType
  def location: Option[ast.AstLocation]
}

object MaterializedType {
  def apply(origin: MatOrigin, tpe: ast.TypeDefinition): MaterializedType =
    MaterializedTypeAst(origin, tpe)
  def apply(origin: MatOrigin, tpe: Type with Named): MaterializedType =
    MaterializedTypeInst(origin, tpe)
}

case class MaterializedTypeAst(origin: MatOrigin, tpe: ast.TypeDefinition)
    extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
  def location = tpe.location
}

case class MaterializedTypeInst(origin: MatOrigin, tpe: Type with Named) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
  def location = None
}

case class BuiltMaterializedTypeInst(origin: MatOrigin, tpe: Type with Named)
    extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
  def location = None
}

sealed trait MaterializedField[Ctx, +Val] {
  def origin: MatOrigin
  def name: String
  def rename(newName: String): MaterializedField[Ctx, Val]
}

object MaterializedField {
  def apply[Ctx](origin: MatOrigin, field: ast.FieldDefinition): MaterializedField[Ctx, Any] =
    MaterializedFieldAst[Ctx](origin, field)

  def apply[Ctx, Val, F[_]](
      origin: MatOrigin,
      field: Field[Ctx, Val, F]): MaterializedField[Ctx, Val, F] =
    MaterializedFieldInst[Ctx, Val, F](origin, field)
}

case class MaterializedFieldAst[Ctx](origin: MatOrigin, field: ast.FieldDefinition)
    extends MaterializedField[Ctx, Any] {
  def name = field.name
  def rename(newName: String) = copy(field = field.copy(name = newName))
}

case class MaterializedFieldInst[Ctx, Val, F[_]](origin: MatOrigin, field: Field[Ctx, Val, F])
    extends MaterializedField[Ctx, Val] {
  def name = field.name
  def rename(newName: String) = copy(field = field.rename(newName))
}
