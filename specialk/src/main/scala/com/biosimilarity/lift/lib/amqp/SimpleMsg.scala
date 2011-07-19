// -*- mode: Scala;-*- 
// Filename:    SimpleMsg.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 11:46:26 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.amqp.utilities

trait AMQPTestUtility[A] {
  case class Msg( a : A, i : Int, b : Boolean, r : Option[Msg] )

  def msgStreamPayload( idx : Int ) : A
  
  var _msgStrm : Option[Stream[Msg]] = None

  def msgStream : Stream[Msg] = {
    _msgStrm match {
      case Some( msgStrm ) => {
	msgStrm
      }
      case None => {
	lazy val msgStrm : Stream[Msg] =
	  List( Msg( msgStreamPayload( 0 ), 0, true, None ) ).toStream append (
	    msgStrm.map(
	      {
		( msg ) => {
		  msg match {
		    case Msg( a, i, b, r ) => {
		      val j = i + 1
		      Msg(
			msgStreamPayload( j ) , j, ((j % 2) == 0), Some( msg )
		      )
		    }
		  }
		}
	      }
	    )
	  )
	_msgStrm = Some( msgStrm )
	msgStrm
      }
    }    
  }
}
