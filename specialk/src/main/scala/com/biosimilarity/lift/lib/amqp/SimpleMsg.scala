// -*- mode: Scala;-*- 
// Filename:    SimpleMsg.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 11:46:26 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.amqp.utilities

trait AMQPTestUtility[A] {
  trait Message {
    def a : A
    def i : Int
    def b : Boolean
    def r : Option[Message]
  }

  case class Msg(
    a : A, i : Int, b : Boolean, r : Option[Message]
  ) extends Message

  def msgStreamPayload( idx : Int ) : A
  
  var _msgStrm : Option[Stream[Message]] = None

  def msgStream(
    mkMsg : (Boolean,Int,A,Option[Message]) => Message
  ) : Stream[Message] = {
    _msgStrm match {
      case Some( msgStrm ) => {
	msgStrm
      }
      case None => {
	lazy val msgStrm : Stream[Message] =
	  List( mkMsg( true, 0, msgStreamPayload( 0 ), None ) ).toStream append (
	    msgStrm.map(
	      {
		( msg ) => {
		  msg match {
		    case Msg( a, i, b, r ) => {
		      val j = i + 1
		      mkMsg(
			((j % 2) == 0),
			j,
			msgStreamPayload( j ),
			Some( msg )
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

  def msgStream : Stream[Message] = {
    msgStream(
      ( b : Boolean, i : Int, a : A, r : Option[Message] ) => {
	Msg( a, i, b, r )
      }
    )
  }
}
