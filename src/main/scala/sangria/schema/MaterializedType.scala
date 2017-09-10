package sangria.schema

import sangria.ast

sealed trait MaterializedType {
  def origin: MatOrigin
  def name: String
  def rename(newName: String): MaterializedType
}

object MaterializedType {
  def apply(origin: MatOrigin, tpe: ast.TypeDefinition): MaterializedType = MaterializedTypeAst(origin, tpe)
  def apply(origin: MatOrigin, tpe: Type with Named): MaterializedType = MaterializedTypeInst(origin, tpe)
}

case class MaterializedTypeAst(origin: MatOrigin, tpe: ast.TypeDefinition) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
}

case class MaterializedTypeInst(origin: MatOrigin, tpe: Type with Named) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
}

case class BuiltMaterializedTypeInst(origin: MatOrigin, tpe: Type with Named) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
}
