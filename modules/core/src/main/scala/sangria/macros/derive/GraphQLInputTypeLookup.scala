package sangria.macros.derive

import sangria.marshalling.FromInput.{InputObjectResult, CoercedScalaResult}
import sangria.util.tag.@@

import language.higherKinds

import sangria.schema._

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Can't find suitable GraphQL input type for ${T}. If you have defined it already, please consider making it implicit and ensure that it's available in the scope.")
trait GraphQLInputTypeLookup[T, G] {
  def graphqlType: InputType[G]
}

object GraphQLInputTypeLookup extends GraphQLInputTypeLookupLowPrio {
  implicit def inCoercedLookup[T](implicit in: InputType[T @@ CoercedScalaResult]) = new GraphQLInputTypeLookup[T, T @@ CoercedScalaResult] {
    def graphqlType = in
  }

  implicit def inObjectLookup[T](implicit in: InputType[T @@ InputObjectResult]) = new GraphQLInputTypeLookup[T, T @@ InputObjectResult] {
    def graphqlType = in
  }

  implicit def optionLookup[T, G](implicit ev: GraphQLInputTypeLookup[T, G]) = new GraphQLInputTypeLookup[Option[T], Option[G]] {
    def graphqlType = OptionInputType(ev.graphqlType)
  }

  def finder[T] = new Finder[T]

  class Finder[T] {
    def apply[G]()(implicit ev: GraphQLInputTypeLookup[T, G]) = ev
  }
}

trait GraphQLInputTypeLookupLowPrio {
  implicit def inLookup[T](implicit in: InputType[T]) = new GraphQLInputTypeLookup[T, T] {
    def graphqlType = in
  }

  implicit def seqLookup[T, Coll[_] <: Seq[_], G](implicit ev: GraphQLInputTypeLookup[T, G]) = new GraphQLInputTypeLookup[Coll[T], Coll[G]] {
    def graphqlType = ListInputType(ev.graphqlType).asInstanceOf[InputType[Coll[G]]]
  }
}

