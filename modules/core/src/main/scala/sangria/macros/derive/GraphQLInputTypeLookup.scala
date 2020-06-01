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
  implicit def inCoercedLookup[T](implicit in: InputType[T @@ CoercedScalaResult]): GraphQLInputTypeLookup[T, T @@ CoercedScalaResult] = new GraphQLInputTypeLookup[T, T @@ CoercedScalaResult] {
    override val graphqlType: InputType[T @@ CoercedScalaResult] = in
  }

  implicit def inObjectLookup[T](implicit in: InputType[T @@ InputObjectResult]): GraphQLInputTypeLookup[T, T @@ InputObjectResult] = new GraphQLInputTypeLookup[T, T @@ InputObjectResult] {
    override val graphqlType: InputType[T @@ InputObjectResult] = in
  }

  implicit def optionLookup[T, G](implicit ev: GraphQLInputTypeLookup[T, G]): GraphQLInputTypeLookup[Option[T], Option[G]] = new GraphQLInputTypeLookup[Option[T], Option[G]] {
    override val graphqlType: OptionInputType[G] = OptionInputType(ev.graphqlType)
  }

  def finder[T] = new Finder[T]

  class Finder[T] {
    def apply[G]()(implicit ev: GraphQLInputTypeLookup[T, G]): GraphQLInputTypeLookup[T, G] = ev
  }
}

trait GraphQLInputTypeLookupLowPrio {
  implicit def inLookup[T](implicit in: InputType[T]): GraphQLInputTypeLookup[T, T] = new GraphQLInputTypeLookup[T, T] {
    override val graphqlType: InputType[T] = in
  }

  implicit def seqLookup[T, Coll[_] <: Seq[_], G](implicit ev: GraphQLInputTypeLookup[T, G]): GraphQLInputTypeLookup[Coll[T], Coll[G]] = new GraphQLInputTypeLookup[Coll[T], Coll[G]] {
    override val graphqlType: InputType[Coll[G]] = ListInputType(ev.graphqlType).asInstanceOf[InputType[Coll[G]]]
  }
}

