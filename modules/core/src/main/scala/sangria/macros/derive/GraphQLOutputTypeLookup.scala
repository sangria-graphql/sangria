package sangria.macros.derive

import language.higherKinds

import sangria.schema._

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Can't find suitable GraphQL output type for ${T}. If you have defined it already, please consider making it implicit and ensure that it's available in the scope.")
trait GraphQLOutputTypeLookup[T] {
  def graphqlType: OutputType[T]
}

object GraphQLOutputTypeLookup extends GraphQLOutputTypeLookupLowPrio {
  implicit def outLookup[T](implicit out: OutputType[T]): GraphQLOutputTypeLookup[T] = new GraphQLOutputTypeLookup[T] {
    override val graphqlType: OutputType[T] = out
  }

  implicit def optionLookup[T : GraphQLOutputTypeLookup]: GraphQLOutputTypeLookup[Option[T]] = new GraphQLOutputTypeLookup[Option[T]] {
    override val graphqlType: OptionType[T] = OptionType(implicitly[GraphQLOutputTypeLookup[T]].graphqlType)
  }
}

trait GraphQLOutputTypeLookupLowPrio {
  implicit def seqLookup[T : GraphQLOutputTypeLookup, Coll[_] <: Seq[_]]: GraphQLOutputTypeLookup[Coll[T]] = new GraphQLOutputTypeLookup[Coll[T]] {
    override val graphqlType: OutputType[Coll[T]] =
      ListType(implicitly[GraphQLOutputTypeLookup[T]].graphqlType).asInstanceOf[OutputType[Coll[T]]]
  }
}


