// -*- mode: Scala;-*- 
// Filename:    TSpaceGenerators.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jun 14 17:25:08 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.design

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import scala.util.continuations._ 

// We assume enough structure that when we introduce types we can
// build a monoidal category of TupleSpaces. More specifically,
// TupleSpaces are morphisms. We make these operations partial and
// introduce the types that say when they are defined in the classes
// below.
trait MTSGenerators[MTS[L,P,R],Place,Pattern,Resource] {
  type CMTS <: MTS[Place,Pattern,Resource]

  // We insist on a co-product-like structure on Patterns
  def inl( p : Pattern ) : Pattern
  def inr( p : Pattern ) : Pattern

  // Isolation
  // (A # B)( inl( k ) ) = A( k )
  // (A # B)( inr( k ) ) = B( k )  
  // Two TupleSpaces, A and B, can be combined so that their keys
  // access completely disjoint value spaces
  def `#`( a : CMTS, b : CMTS ) : CMTS

  // Note that the co-product structure naturally generalizes to an
  // indexed structure -- thus producing the partitioned junctions
  // that were hand-coded previously

  // Addition
  // (A ++ B)( k ) = A( k ) ++ B( k )
  // Two TupleSpaces, A and B, can be combined so that keys with
  // overlapping values combine the value space
  def ++( a : CMTS, b : CMTS ) : CMTS

  // Composition
  // (A o B)( k ) = U_{k' in A( k )} B( k' )
  // Two TupleSpaces, A and B, can be combined in a manner that mimics
  // relational composition. A key, k, accesses values in A that are
  // used as keys in B. The value collections are combined.
  def `o`( a : CMTS, b : CMTS ) : CMTS

  // Key difference
  // (A - B)( k ) =
  //   A( k ) if B( k ) = None or B( k ) if A( k ) = None or None
  // Two TupleSpaces, A and B, can be combined via symmetric
  // difference of key sets
  def `-`( a : CMTS, b : CMTS ) : CMTS

  // Difference
  // (A -- B)( k ) = A( k ) -- B( k )
  // Two TupleSpaces, A and B, can be combined so that value
  // collections are differenced
  def `--`( a : CMTS, b : CMTS ) : CMTS
}

trait PatternTyping[Place,Pattern,PtnType] {
  // A pattern can be checked for inhabitance of a type
  def in( p : Pattern, t : PtnType ) : Boolean 
  
  // A pattern can be queried for a principal type
  def principal( p : Pattern ) : Option[PtnType]
}

trait MonadicTSTyping[MTS[L,P,R],Place,Pattern,Resource,PtnType] {
  self : PatternTyping[Place,Pattern,PtnType] =>

  // A scope says whether a client uses the I or the O patterns.
  type Scope[
    MTSP,
    MTSPattern,
    U
  ] <: MTS[MTS[Place,Pattern,Resource],MTSPattern,U]

  // Note that because a scope is an MTS it is able to partake of the
  // operations structure above. One of the elements that needs to be
  // fleshed out in this situation is the relationshipe between scope
  // nesting and name structure
  type ScopeCompositors[
    MTSP,
    MTSPattern,
    U
  ] <: MTSGenerators[Scope,MTS[Place,Pattern,Resource],MTSPattern,U]
  
  // A type describes patterns governing the use of the space over the
  // following matrix of values
  // ------------------------------------
  //          | In scope | Out of scope |
  // ------------------------------------
  // Consumer |          |              |
  // ------------------------------------
  // Producer |          |              |
  // ------------------------------------
  trait TSType {
    def inScopeConsumer : PtnType
    def outScopeConsumer : PtnType
    def inScopeProducer : PtnType
    def outScopeProducer : PtnType
  }
  case class StdTSType(
    inScopeConsumer : PtnType,
    outScopeConsumer : PtnType,
    inScopeProducer : PtnType,
    outScopeProducer : PtnType
  ) extends TSType

  // A typed TupleSpace governs the verbs with types
  trait TypedTupleSpace[L,P,R] {
    def space : MTS[L,P,R]
    def `type` : TSType
    
    // These all follow the same pattern -- so, there is a higher-order
    // abstraction that can be pulled out. More importantly, these
    // checks need to be done at compile time (via a compiler
    // plugin?), not at runtime...
//   = {
//    scope.get( space ) match {
// 	case Some( _ ) => {
// 	  in( ptn, `type`.inScopeConsumer ) match {
// 	    case true => space.get( ptn )
// 	    case false => None /* Alternately, throw an exception */
// 	  }
// 	}
// 	case _ => {
// 	  in( ptn, `type`.outScopeConsumer ) match {
// 	    case true => space.get( ptn )
// 	    case false => None /* Alternately, throw an exception */
// 	  }
// 	}
//       }
//    }

    def get [MTSPattern,U] (
      ptn : Pattern, scope : Scope[MTS[L,P,R],MTSPattern,U]
    ) : Option[Resource] 

    def fetch [MTSPattern,U] (
      ptn : Pattern, scope : Scope[MTS[L,P,R],MTSPattern,U]
    ) : Option[Resource]
    def subscribe [MTSPattern,U] (
      ptn : Pattern, scope : Scope[MTS[L,P,R],MTSPattern,U]
    ) : Option[Resource]

    // These all follow the same pattern -- so, there is a higher-order
    // abstraction that can be pulled out. More importantly, these
    // checks need to be done at compile time (via a compiler
    // plugin?), not at runtime...

//   = {
//    scope.get( space ) match {
// 	case Some( _ ) => {
// 	  in( ptn, `type`.inScopeProducer ) match {
// 	    case true => space.put( ptn, rsrc )
// 	    case false => /* Alternately, throw an exception */
// 	  }
// 	}
// 	case _ => {
// 	  in( ptn, `type`.outScopeProducer ) match {
// 	    case true => space.put( ptn, rsrc )
// 	    case false => /* Alternately, throw an exception */
// 	  }
// 	}
//       }
//    }
    def put [MTSPattern,U] (
      ptn : Pattern, rsrc : Resource, scope : Scope[MTS[L,P,R],MTSPattern,U]
    ) : Unit  

    def publish [MTSPattern,U] (
      ptn : Pattern, rsrc : Resource, scope : Scope[MTS[L,P,R],MTSPattern,U]
    ) : Unit
  }

  abstract class StdTypedTupleSpace[L,P,R](
    val space : MTS[L,P,R],
    val `type` : TSType
  ) extends TypedTupleSpace[L,P,R]
}


