package sangria.validation

import sangria.schema._

object TypeComparators {
  def isEqualType(type1: Type, type2: Type): Boolean =
    (type1, type2) match {
      case (OptionType(t1), OptionType(t2)) ⇒ isEqualType(t1, t2)
      case (OptionInputType(t1), OptionInputType(t2)) ⇒ isEqualType(t1, t2)
      case (ListType(t1), ListType(t2)) ⇒ isEqualType(t1, t2)
      case (ListInputType(t1), ListInputType(t2)) ⇒ isEqualType(t1, t2)
      case (t1: Named, t2: Named) ⇒ t1.name == t2.name
      case _ ⇒ false
    }

  def isSubType(schema: Schema[_, _], subType: Type, superType: Type): Boolean =
    (subType, superType) match {
      case (OptionType(ofType1), OptionType(ofType2)) ⇒ isSubType(schema, ofType1, ofType2)
      case (OptionInputType(ofType1), OptionInputType(ofType2)) ⇒ isSubType(schema, ofType1, ofType2)
      case (sub, OptionInputType(ofType2)) ⇒ isSubType(schema, sub, ofType2)
      case (ListType(ofType1), ListType(ofType2)) ⇒ isSubType(schema, ofType1, ofType2)
      case (ListInputType(ofType1), ListInputType(ofType2)) ⇒ isSubType(schema, ofType1, ofType2)
      case (t1: ObjectType[_, _], t2: AbstractType) ⇒ schema.isPossibleType(t2.name, t1)
      case (t1: Named, t2: Named) ⇒ t1.name == t2.name
      case _ ⇒ false
    }
}
