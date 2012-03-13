// -*- mode: Scala;-*- 
// Filename:    Generators.scala 
// Authors:     lgm                                                    
// Creation:    Sat May  2 23:01:36 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.scalar

import scala.collection.SeqProxy

import java.net.URL

trait Expressions {
  type Nominal
  abstract class Expression
  trait Value[A] { def value : A }
  abstract class SeqLiteral( override val value : List[Expression] )
       extends Expression with Value[List[Expression]] with SeqProxy[Expression] {
	 def self = value
       }
  case class ListLiteral( override val value : List[Expression] )
       extends SeqLiteral( value )
  case class SummationLiteral( override val value : List[Expression] )
       extends SeqLiteral( value )
  case class MultiplicationLiteral( override val value : List[Expression] )
       extends SeqLiteral( value )
  case class NegationLiteral( override val value : Expression )
       extends Expression with Value[Expression]
  case class IntegerLiteral( override val value : Int )
       extends Expression with Value[Int]
  case class DoubleLiteral( override val value : Double )
       extends Expression with Value[Double]
  case class StringLiteral( override val value : String )
       extends Expression with Value[String]
  case class BooleanLiteral( override val value : Boolean )
       extends Expression with Value[Boolean]
  case object BottomLiteral
       extends Expression with Value[Unit] {	 
	 override def value : Unit = {
	   throw new Exception( "reached bottom" )
	 }
       }
  case class Mention( reference : Nominal )
       extends Expression
  case class Abstraction( formals : List[Nominal], body : Expression )
       extends Expression
  case class Application( operation : Expression, actuals : List[Expression] )
       extends Expression

  // non-standard term structure
  case class Dereference( reference : Nominal ) extends Expression

  def freeVariables( term : Expression ) : Set[Nominal] = {
    term match {      
      case Mention( reference ) => Set( reference )
      case Abstraction( formals, body ) =>
	freeVariables( body ) &~ formals.toSet
      case Application( operation, actuals ) =>
	( freeVariables( operation ) /: actuals )(
	  { ( acc, elem ) => acc ++ freeVariables( elem ) } 
	)
      case v : Value[_] => Set( )
    }
  }
}

trait Nominals {
  type Term
  abstract class Name
  case class Transcription( expression : Term )
       extends Name
  case class StringVariable( str : String )
       extends Name
  case class DeBruijn( outerIndex : Int, innerIndex : Int )
       extends Name
  case class URLVariable( url : java.net.URL )
       extends Name

  def asName( a : Any ) = {
      a match {
	case t : Term => Transcription( t )
	case s : String => StringVariable( s )
	case u : java.net.URL => URLVariable( u )
	case p : ( Int, Int ) => {
	  p match { case ( i, j ) => DeBruijn( i, j) }
	}
	case _ => throw new Exception( "no nominal conversion" )
      }
    }

  def nameStream( seed : Any )( fresh : Name => Name  ) = {
    lazy val loopStrm : Stream[Name] =
      (List( asName( seed ) ).toStream append (loopStrm map fresh));
    loopStrm
  }
}

// Instantiating the abstract syntax framework

trait ReflectiveGenerators extends Expressions with Nominals {
  type Nominal = Transcription
  type Term = Expression
}

// An example of the reflective abstract syntax
object theReflectiveGeneration extends ReflectiveGenerators {
  // Can you say namespaces? i knew you could...
  val countingNames : Stream[Name] =
    nameStream( 0 ){
      ( n : Name ) =>
	n match {
	  case Transcription( IntegerLiteral( i ) ) =>
	    Transcription( IntegerLiteral( i + 1 ) )
	  case _ => throw new Exception( "unexpected nominal type" )
	}
    }
}

trait StringGenerators extends Expressions with Nominals {
  type Nominal = StringVariable
  type Term = Expression
}

// An example of the string-based abstract syntax
object theStringGeneration extends StringGenerators {
  // Another example of a namespace
  val kleeneNames : Stream[Name] =
    nameStream( "a" ){
      ( n : Name ) =>
	n match {
	  case StringVariable( s ) =>
	    StringVariable( "a" + s )
	  case _ => throw new Exception( "unexpected nominal type" )
	}
    }
}

trait URLGenerators extends Expressions with Nominals {
  type Nominal = URLVariable
  type Term = Expression
}

// An example of the deBruijn-based syntax
trait DeBruijnGenerators extends Expressions with Nominals {
  type Nominal = DeBruijn
  type Term = Expression
}

object theDeBruijnGeneration extends DeBruijnGenerators {
  // Another example of a namespace
  val otherCountingNames : Stream[Name] =
    nameStream( ( 0, 0 ) ){
      ( n : Name ) =>
	n match {
	  case DeBruijn( i, j ) => DeBruijn( i + 1, j )
	  case _ => throw new Exception( "unexpected nominal type" )
	}
    }
}

trait RichGenerators extends Expressions with Nominals {
  type Nominal = Name
  type Term = Expression  
}

// An example of the a syntax over all four types of nominals
object theRichGeneration extends RichGenerators {
  // Another example of a namespace
  // All these examples illustrate that serious attention to the
  // structure of names affords a rich algebra of namespaces
  val richNameStream : Stream[Nominal] =
    nameStream( 0 ){
      ( n : Name ) =>
	n match {
	  case Transcription( IntegerLiteral( i ) ) =>
	    StringVariable( i.toString )
	  case StringVariable( s ) =>
	    DeBruijn( s.toInt, 0 )
	  case DeBruijn( i, j ) =>
	    Transcription( IntegerLiteral( i + j ) )
	  case URLVariable( url ) =>
	    URLVariable( new java.net.URL( url, n.toString ) )
	  case _ =>
	    throw new Exception( "unexpected nominal type" )
	}
    }
}
