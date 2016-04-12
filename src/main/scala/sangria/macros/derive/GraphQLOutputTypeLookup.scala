package sangria.macros.derive

import language.higherKinds

import sangria.schema._

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Can't find suitable GraphQL output type for ${T}. If you have defined it already, please consider making it implicit and ensure that it's available in the scope.")
trait GraphQLOutputTypeLookup[T] {
  def graphqlType: OutputType[T]
}

object GraphQLOutputTypeLookup extends GraphQLOutputTypeLookupLowPrio {
  implicit def outLookup[T](implicit out: OutputType[T]) = new GraphQLOutputTypeLookup[T] {
    def graphqlType = out
  }

  implicit def optionLookup[T : GraphQLOutputTypeLookup]  = new GraphQLOutputTypeLookup[Option[T]] {
    def graphqlType = OptionType(implicitly[GraphQLOutputTypeLookup[T]].graphqlType)
  }
}

trait GraphQLOutputTypeLookupLowPrio {
  implicit def seqLookup[T : GraphQLOutputTypeLookup, Coll[_] <: Seq[_]] = new GraphQLOutputTypeLookup[Coll[T]] {
    def graphqlType =
      ListType(implicitly[GraphQLOutputTypeLookup[T]].graphqlType).asInstanceOf[OutputType[Coll[T]]]
  }
}


