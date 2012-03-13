// -*- mode: Scala;-*- 
// Filename:    Reduction.scala 
// Authors:     lgm                                                    
// Creation:    Sun May  3 01:15:07 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.scalar

trait Values {
  type Environment
  type Expression
  abstract class Value
  case class Closure(
    fn : List[Value] => Value
  ) extends Value
  case class Quantity( quantity : Int )
       extends Value
}

trait Reduction extends Expressions with Values {

  type Dereferencer = {def apply( m : Mention ) : Value }
  type Expansionist =
    {def extend( fmls : List[Mention], actls : List[Value] ) : Dereferencer}
  type Environment <: (Dereferencer with Expansionist)
  type Applicator = Expression => List[Value] => Value

  val initialApplicator : Applicator =
    { ( xpr : Expression ) => {
	  ( actls : List[Value] ) => {
	    xpr match {
	      case IntegerLiteral( i ) => Quantity( i )
	      case _ => throw new Exception( "why are we here?" )
	    }
	  }
	}
      }

  def reduce(
    applicator : Applicator,
    environment : Environment
  ) : Expression => Value = { 
    case IntegerLiteral( i ) => Quantity( i )
    case Mention( v ) => environment( Mention( v ) )
    case Abstraction( fmls, body ) =>
      Closure(
	{ ( actuals : List[Value] ) => {
	  val keys : List[Mention] =
 	    fmls.map( { ( fml : Nominal ) => Mention( fml ) });
	  reduce(
	    applicator,
	    environment.extend( keys, actuals ).asInstanceOf[Environment]
	  )( body )
	  }
	 }
      )
    case Application(
      operator : Expression,
      actuals : List[Expression] ) => {
	reduce( applicator, environment )( operator ) match {
	  case Closure( fn ) => {
	    fn.apply(
	      (actuals
	       map
	       {( actual : Expression) =>
		 (reduce( applicator, environment ))( actual )})
	    )
	  }
	  case _ => throw new Exception( "attempt to apply non function" )
	}
      }
    case _ => throw new Exception( "not implemented, yet" )
  }

}

object DeBruijnReduction extends Reduction with Nominals {
  type Nominal = DeBruijn
  type Term = Expression
  type Environment = DeBruijnEnvironment

  case class DeBruijnEnvironment( rail : List[List[Value]] ) {
    def apply( mention : Mention ) = {
      mention match {
	case Mention( DeBruijn( i, j ) ) =>
	  rail(i)(j)
      }
    }
    def extend(
      fmls : List[Mention],
      actls : List[Value]
    ) = {      
      new DeBruijnEnvironment( actls :: rail )
    }
  }
}

object AssociativeReduction extends Reduction with Nominals {
  type Nominal = StringVariable
  type Term = Expression
  type Environment = AssociativeEnvironment

  case class AssociativeEnvironment( map : scala.collection.immutable.Map[ Mention, Value ] ) {
    def apply( mention : Mention ) = {
      map.get( mention ) match {
	case Some( v )  => v
	case None => throw new Exception( "not found" )
      }
    }
    def extend(
      fmls : List[Mention],
      actls : List[Value]
    ) = {
      val pairs : List[(Mention,Value)] = fmls.zip( actls );
      new AssociativeEnvironment( map ++ pairs )
    }
  }
}
