package sangria.schema

import sangria.execution.ValueCoercionHelper
import sangria.marshalling.{CoercedScalaResultMarshaller, ToInput}
import sangria.renderer.SchemaRenderer
import sangria.validation._

trait SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]): List[Violation]
}

object SchemaValidationRule {
  val empty: List[SchemaValidationRule] = Nil
  val default: List[SchemaValidationRule] = new DefaultValuesValidationRule :: new InterfaceImplementationValidationRule :: Nil
}

class DefaultValuesValidationRule extends SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]) = {
    val coercionHelper = ValueCoercionHelper.default

    def validate(prefix: ⇒ String, path: List[String], tpe: InputType[_])(defaultValue: (_, ToInput[_, _])) = {
      val (default, toInput) = defaultValue.asInstanceOf[(Any, ToInput[Any, Any])]
      val (inputValue, iu) = toInput.toInput(default)

      coercionHelper.coerceInputValue(tpe, path, inputValue, None, CoercedScalaResultMarshaller.default, CoercedScalaResultMarshaller.default, prefix)(iu) match {
        case Left(violations) ⇒ violations
        case Right(violations) ⇒ Nil
      }
    }

    val inputTypeViolations = schema.inputTypes.values.toList flatMap {
      case it: InputObjectType[_] ⇒
        it.fields flatMap (f ⇒
          f.defaultValue map validate(s"Invalid default value of field '${f.name}' in input type '${it.name}'. ", it.name :: f.name :: Nil, f.inputValueType) getOrElse Nil)
      case _ ⇒ Nil
    }

    val outputTypeViolations = schema.outputTypes.values.toList flatMap {
      case ot: ObjectLikeType[_, _] ⇒
        ot.fields flatMap (f ⇒
          f.arguments flatMap (a ⇒
            a.defaultValue map validate(s"Invalid default value of argument '${a.name}' in field '${f.name}' defined in output type '${ot.name}'. ", ot.name :: f.name :: ("[" + a.name + "]") :: Nil, a.inputValueType) getOrElse Nil))
      case _ ⇒ Nil
    }

    inputTypeViolations ++ outputTypeViolations
  }
}

class InterfaceImplementationValidationRule extends SchemaValidationRule {
  private def validateObjectType[Ctx, Val](schema: Schema[Ctx, Val], objTpe: ObjectType[_, _], intTpe: InterfaceType[_, _]): List[Violation] = {
    val objFields: Map[String, Vector[Field[_, _]]] = objTpe.ownFields.groupBy(_.name)

    intTpe.ownFields.toList.flatMap { intField ⇒
      objFields.get(intField.name) match {
        case None ⇒
          // we allow object type to inherit fields from the interfaces
          // without explicitly defining them
          Nil

        case Some(objField) if !TypeComparators.isSubType(schema, objField.head.fieldType, intField.fieldType) ⇒
          InvalidImplementationFieldTypeViolation(
            intTpe.name,
            objTpe.name,
            intField.name,
            SchemaRenderer.renderTypeName(intField.fieldType),
            SchemaRenderer.renderTypeName(objField.head.fieldType)) :: Nil

        case Some(objField) ⇒
          val intArgViolations = intField.arguments.flatMap { iarg ⇒
            objField.head.arguments.find(_.name == iarg.name) match {
              case None ⇒
                MissingImplementationFieldArgumentViolation(intTpe.name, objTpe.name, intField.name, iarg.name) :: Nil

              case Some(oarg) if !TypeComparators.isEqualType(iarg.argumentType, oarg.argumentType) ⇒
                InvalidImplementationFieldArgumentTypeViolation(
                  intTpe.name,
                  objTpe.name,
                  intField.name,
                  iarg.name,
                  SchemaRenderer.renderTypeName(iarg.argumentType),
                  SchemaRenderer.renderTypeName(oarg.argumentType)) :: Nil

              case _ ⇒ Nil
            }
          }

          val objArgViolations = objField.head.arguments
            .filterNot(oa ⇒ intField.arguments.exists(_.name == oa.name))
            .flatMap {
              case oarg if !oarg.argumentType.isInstanceOf[OptionInputType[_]] ⇒
                ImplementationExtraFieldArgumentNotOptionalViolation(
                  intTpe.name,
                  objTpe.name,
                  intField.name,
                  oarg.name,
                  SchemaRenderer.renderTypeName(oarg.argumentType)) :: Nil
              case _ ⇒ Nil
            }

          intArgViolations ++ objArgViolations
      }
    }
  }

  def validate[Ctx, Val](schema: Schema[Ctx, Val]) =
    schema.possibleTypes.toList.flatMap {
      case (intName, objTypes) ⇒
        schema.outputTypes(intName) match {
          case intTpe: InterfaceType[_, _] ⇒ objTypes.flatMap(validateObjectType(schema, _, intTpe))
          case _ ⇒ Nil
        }
    }
}

case class SchemaValidationException(violations: List[Violation]) extends IllegalArgumentException {
  override lazy val getMessage =
    "Schema contains validation errors.\n" + violations.map(v ⇒ "  * " + v.errorMessage).mkString("\n")
}
