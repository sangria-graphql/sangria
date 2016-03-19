package sangria.macros.derive

import language.existentials

import sangria.schema.InputField

trait DeriveInputObjectSetting

case class InputObjectTypeName(name: String) extends DeriveInputObjectSetting
case class InputObjectTypeDescription(description: String) extends DeriveInputObjectSetting

case class DocumentInputField(fieldName: String, description: String) extends DeriveInputObjectSetting
case class RenameInputField(fieldName: String, graphqlName: String) extends DeriveInputObjectSetting
case class ReplaceInputField(fieldName: String, field: InputField[_]) extends DeriveInputObjectSetting

case class IncludeInputFields(fieldNames: String*) extends DeriveInputObjectSetting
case class ExcludeInputFields(fieldNames: String*) extends DeriveInputObjectSetting
