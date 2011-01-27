// -*- mode: Scala;-*- 
// Filename:    Slog.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 11:17:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import scala.xml._
import java.util.UUID

trait WireTap {
  def tap [A] ( fact : A ) : Unit
}

trait Journalist {

  object journalIDVender extends UUIDOps

  trait Verbosity {
    def id : UUID
  }
  class Luddite( override val id : UUID ) extends Verbosity
  class Blogger( override val id : UUID ) extends Luddite( id )
  class Twitterer( override val id : UUID ) extends Blogger( id )

  object Twitterer {
    def apply( id : UUID ) : Twitterer = new Twitterer( id )
    def unapply( t : Twitterer ) : Option[(UUID)] = Some( (t.id) )
  }

  object Blogger {
    def apply( id : UUID ) : Blogger = new Blogger( id )
    def unapply( b : Blogger ) : Option[(UUID)] = Some( (b.id) )
  }

  object Luddite {
    def apply( id : UUID ) : Luddite = new Luddite( id )
    def unapply( l : Luddite ) : Option[(Unit)] = Some( (l.id) )
  }

  case object TheTwitterer extends Twitterer( journalIDVender.getUUID )
  case object TheBlogger extends Blogger( journalIDVender.getUUID )
  case object TheLuddite extends Luddite( journalIDVender.getUUID )

  val reportage = report( TheTwitterer ) _

  case class TaggedFact[A]( verb : Verbosity, fact : A )

  def markUp[A]( verb : Verbosity)( fact : A ) =
    TaggedFact( verb, fact )

  def asTweet[A]( fact : A ) = markUp[A]( TheTwitterer ) _
  def asBlog[A]( fact : A ) = markUp[A]( TheBlogger ) _
  def asSilence[A]( fact : A ) = markUp[A]( TheLuddite ) _

  def tweet[A]( fact : A ) = report( TheTwitterer )( asTweet( fact ) )
  def blog[A]( fact : A ) = report( TheBlogger )( asTweet( fact ) )
  def silence[A]( fact : A ) = report( TheLuddite )( asTweet( fact ) )

  def report [A] ( verb : Verbosity )( fact : A ) : Unit = {
    fact match {
      case TaggedFact( vrb, bite ) => {
	if ( vrb.isInstanceOf[verb.type] ) {
	  val rpt = <report>{bite}</report>
	  println( rpt )
	}
      }
      case _ => {
	verb match {
	  case Twitterer( _ ) => {
	    val rpt = <report>{fact}</report>
	    println( rpt )
	  }
	  case Blogger( _ ) => {
	    val rpt = <report>{fact}</report>
	    println( rpt )
	  }
	  case Luddite( _ ) => 
	}
      }
    }    
  }
}
