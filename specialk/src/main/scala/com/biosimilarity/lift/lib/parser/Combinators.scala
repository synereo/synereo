// -*- mode: Scala;-*- 
// Filename:    Combinators.scala 
// Authors:     lgm                                                    
// Creation:    Sat Mar 12 04:01:33 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.parser
import com.biosimilarity.lift.lib.monad._
import com.biosimilarity.lift.lib.delimited._

import scala.collection.immutable.Stream

// A parser consumes streams of tokens and produces ...
trait Parser[Token,Result]

// ... a list of pairs of results and streams of tokens
class CParser[Token,Result](
  val parse : Stream[Token] => List[(Result,Stream[Token])]
) extends Parser[Token,Result]

object CParser {
  def apply [Token,Result] (
    parse : Stream[Token] => List[(Result,Stream[Token])]
  ) : CParser[Token,Result] = {
    new CParser( parse )
  }
  def unapply [Token,Result] (
    cp : CParser[Token,Result]
  ) : Option[( Stream[Token] => List[(Result,Stream[Token])] )] = {
    Some( cp.parse )
  }
}

// The fact that a parser may be interpreted as a monad must be
  // addressed in a type-lambda which abstracts in the type Token to
  // obtain a version of the parser type, MCParser, with a single type
  // parameter, Result. This allows us to construct the witness of
  // MCParser's monadicity.
trait MonadicParserWitnessScope[Result] {
  type Token
  class MCParser[Result](
    val mcp : Stream[Token] => List[(Result,Stream[Token])]
  ) extends CParser[Token,Result]( mcp )

  class MonadicParserWitness[Result](
    val cparser : MCParser[Result] 
  ) extends Monad[MCParser] {

    // a function from A to B lifts naturally to a function from
      // parsers resulting in A to parsers resulting in B
    override def fmap [A,B] (
      f : A => B
    ) : MCParser[A] => MCParser[B] = {
      ( mcpA ) => {
	new MCParser[B]( 
	  ( stkn ) => mcpA.mcp( stkn ).map(
	    { ( rs ) => ( f( rs._1 ), rs._2 ) }
	  )
	)
      }
    }

    // the unit of the parser monad ignores the stream of tokens and
      // produces the boxed result
    override def unit [A] ( a : A ) : MCParser[A] = {
      new MCParser[A]( ( stkn ) => List( ( a, stkn ) ) )
    }

    // the mult is defined in terms of bind this time
    override def mult [A] (
      mma : MCParser[MCParser[A]]
    ) : MCParser[A] = {
      bind[MCParser[A],A]( mma, ( ma ) => ma )
    }

    // the bind is a more natural primitive in this case because of
      // the sequential nature of combining parsers
    override def bind [A,B] (
      ma : MCParser[A], f : A => MCParser[B]
    ) : MCParser[B] = {
      new MCParser[B]( 
	( stkn ) => {
	  ma.parse( stkn ).flatMap( 
	    {
	      ( rnstkn ) => {
		f( rnstkn._1 ).parse( rnstkn._2 )
	      }
	    }
	  )
	}
      )
    }
  }

  class BMonadicParserWitness[Result](
    val cparser : MCParser[Result] 
  ) extends BMonad[MCParser] {

    // the unit of the parser monad ignores the stream of tokens and
      // produces the boxed result
    override def unit [A] ( a : A ) : MCParser[A] = {
      new MCParser[A]( ( stkn ) => List( ( a, stkn ) ) )
    }

    // the bind is a more natural primitive in this case because of
      // the sequential nature of combining parsers
    override def bind [A,B] (
      ma : MCParser[A], f : A => MCParser[B]
    ) : MCParser[B] = {
      new MCParser[B]( 
	( stkn ) => {
	  ma.parse( stkn ).flatMap( 
	    {
	      ( rnstkn ) => {
		f( rnstkn._1 ).parse( rnstkn._2 )
	      }
	    }
	  )
	}
      )
    }
  }
  
  class MonadPlusParserWitness[Result](
    override val cparser : MCParser[Result] 
  ) extends MonadicParserWitness[Result](
      cparser
    ) with MonadPlus[MCParser] {
    override def zero [A] : MCParser[A] = {
      new MCParser[A](
	( stkn ) => Nil
      )
    }
    override def plus [A] (
      ma1 : MCParser[A], ma2 : MCParser[A]
    ) : MCParser[A] = {
      new MCParser[A](
	( stkn ) => {
	  ma1.parse( stkn ) ++ ma2.parse( stkn )
	}
      )
    }
  }  
}

trait MonadicScopeComposition[
  Abstraction[FRslt] <: MonadicParserWitnessScope[FRslt],
  Argument[ARslt] <: MonadicParserWitnessScope[ARslt],
  ARslt,
  FRslt
] extends MonadicParserWitnessScope[FRslt] {
  val abstractionScope : Abstraction[FRslt]
  val argumentScope : Argument[ARslt]
  type Token = ARslt
}

trait MonadicParsingCoreUtilities[Result]
  extends MonadicParserWitnessScope[Result] {
    trait MonadicParsingToolBox[Result] {
      self : MonadPlusParserWitness[Result] =>
       
    def item : MCParser[Token] = {
      new MCParser[Token](
	( stkn ) => {
	  if ( stkn.isEmpty ) {
	    Nil
	  }
	  else {
	    List( ( stkn.take( 1 )( 0 ), stkn.drop( 1 ) ) )
	  }
	}
      )
    }

    def sat( filter : Token => Boolean ) = {
      bind[Token,Token](
	item,
	( t : Token ) => {
	  if ( filter( t ) ) {
	    unit[Token]( t )
	  }
	  else zero[Token]
	}
      )
    }
  } 
}

trait MonadicCharacterParsingUtilities[Result]
  extends MonadicParsingCoreUtilities[Result]
{
  type Token = scala.runtime.RichChar
  trait MonadicParsingCharacterToolBox[Result] {
    self : MonadicParsingToolBox[Result] =>
      import java.util.regex.Pattern
    def ch( c : scala.runtime.RichChar ) : MCParser[scala.runtime.RichChar] = {
	sat( _ == c )
      }
    def upper : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isUpper )
    }
    def lower : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isLower )
    }
    def alpha : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isLetter )
    }
    def digit : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isDigit )
    }
    def alnum : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isLetterOrDigit )
    }
    def punct : MCParser[scala.runtime.RichChar] = {
      sat(
	( c : scala.runtime.RichChar ) =>
	  Pattern.matches( "\\p{Punct}", c.toString )
      )
    }
    def white : MCParser[scala.runtime.RichChar] = {
      sat( ( c : scala.runtime.RichChar ) => c.isWhitespace )
    }   
  } 
}
