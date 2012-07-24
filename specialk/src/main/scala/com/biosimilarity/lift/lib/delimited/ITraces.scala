// -*- mode: Scala;-*- 
// Filename:    interleavedTraces.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jul 20 23:54:03 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.qna

import com.biosimilarity.lift.lib._

import scala.collection.immutable.Stream
import scala.collection.immutable.Stream.Cons

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import scala.util.parsing.combinator._

// C ::= ( S )*
// S ::= [ C ]*

trait CSData {
  // Data representation of this idea
  trait Question
  trait Answer
  
  case object CO extends Question {
    override def toString : String = "("
  }
  case object CC extends Answer {
    override def toString : String = ")"
  }
  case object SO extends Question {
    override def toString : String = "["
  }
  case object SC extends Answer {
    override def toString : String = "]"
  }

  abstract class QAStream[+Q <: Question, +A <: Answer, +S <: QAStream[Question,Answer,_]] extends Stream[( Q, S, A )]

  case class CStream( data : Cons[( CO.type, SStream, CC.type )] ) extends QAStream[CO.type, CC.type, SStream] {
    override def tailDefined = data.tailDefined
    override def head = data.head
    override def tail = data.tail
    override def isEmpty = data.isEmpty
  }
  case class SStream( data : Cons[( SO.type, CStream, SC.type )] ) extends QAStream[ SO.type, SC.type, CStream] {
    override def tailDefined = data.tailDefined
    override def head = data.head
    override def tail = data.tail
    override def isEmpty = data.isEmpty
  }
}

trait CSControl {
  self : CSData =>

  // But there must be an adapter between that and the actual world  
  
  abstract class GamePlay extends Stream[Either[Question,Answer]]

  case class GameHistory( trace : Stream[Either[Question,Answer]] ) extends GamePlay {
    override def tailDefined = true
    override def head = trace.head
    override def tail = GameHistory( trace.tail )
    override def isEmpty = trace.isEmpty
  }    

  def handleClientStream(
    history : GameHistory,
    oSK : Option[GameHistory => GameHistory],
    oCK : Option[GameHistory => GameHistory]
  )( loop : Boolean ) : GameHistory = {
    history.take( 1 )( 0 ) match {
      case Left( qn ) => {
	qn match {
	  case CO => {
	    print( CO )
	    reset {
	      shift {
		( k : GameHistory => GameHistory ) => {
		  val revisedHistory =
		    handleServerStream( history.drop( 1 ).asInstanceOf[GameHistory], Some( k ), oCK )( loop )
		  oCK match {
		    case Some( cK ) => cK( revisedHistory )
		    case None => {
		      if ( loop ) {
			handleClientStream( revisedHistory, oSK, oCK )( loop )
		      }
		      else {
			revisedHistory
		      }
		    }
		  }
		}
	      }
	    }
	  }
	  case SO => {
	    throw new Exception( "unexpected move: " + SO )
	  }
	}
      }
      case Right( ans ) => {
	ans match {
	  case SC => {
	    oSK match {
	      case Some( sK ) => {
		print( SC )
		sK( history.drop( 1 ).asInstanceOf[GameHistory] )
	      }
	      case None => {
		throw new Exception( "unexpected move: " + CC )
	      }
	    }
	  }
	  case CC => {
	    throw new Exception( "unexpected move: " + SC )
	  }
	}
      }
    }
  }

  def handleServerStream( 
    history : GameHistory,
    oCK : Option[GameHistory => GameHistory],
    oSK : Option[GameHistory => GameHistory]
  )( loop : Boolean ) : GameHistory = {
    history.take( 1 )( 0 ) match {
      case Left( qn ) => {
	qn match {
	  case SO => {
	    print( SO )
	    reset {
	      shift {
		( k : GameHistory => GameHistory ) => {
		  val revisedHistory =
		    handleClientStream( history.drop( 1 ).asInstanceOf[GameHistory], Some( k ), oCK )( loop )
		  oCK match {
		    case Some( cK ) => cK( revisedHistory )
		    case None => {
		      if ( loop ) {
			handleServerStream( revisedHistory, oCK, oSK )( loop )
		      }
		      else {
			revisedHistory
		      }
		    }
		  }		  
		}
	      }
	    }
	  }
	  case CO => {
	    throw new Exception( "unexpected move: " + SO )
	  }
	}
      }
      case Right( ans ) => {
	ans match {
	  case CC => {
	    oCK match {
	      case Some( cK ) => {
		print( CC )
		cK( history.drop( 1 ).asInstanceOf[GameHistory] )
	      }
	      case None => {
		throw new Exception( "unexpected move: " + CC )
	      }
	    }
	  }
	  case SC => {
	    throw new Exception( "unexpected move: " + SC )
	  }
	}
      }
    }
  }
}

trait CSIO {
  self : CSData =>
  class StreamMaker extends JavaTokenParsers {
    def foldTrace( lStr : List[Stream[Either[Question,Answer]]] ) : Stream[Either[Question,Answer]] = {
      ( List[Either[Question,Answer]]( ).toStream /: lStr )( 
	( acc : Stream[Either[Question,Answer]], e : Stream[Either[Question,Answer]] ) => {
	  e ++ acc
	}
      )
    }
    def clientTrace : Parser[List[Stream[Either[Question,Answer]]]] =
      ">>"~repsep( "("~serverTrace~")", ":" )~">>" ^^ {
	case ">>"~clientTraces~">>" => {
	  clientTraces match {
	    case "("~serverTrace~")" :: cTraces => {
	      val cTrs =
		( List[Stream[Either[Question,Answer]]]( ) /: cTraces )( 
		  ( acc, e ) => {
		    e match {
		      case "("~serverTrace~")" => { serverTrace ++ acc }
		    }
		  }
		)
	      List[Stream[Either[Question,Answer]]](
		(
		  List( Left[Question,Answer]( CO ) ).toStream
		  append ( foldTrace( serverTrace ) )
		  append List( Right[Question,Answer]( CC ) ).toStream		  
		)
	      ) ++ cTrs
	    }
	    case Nil => {
	      List[Stream[Either[Question,Answer]]]( )
	    }
	  }
	}	
      }

    def serverTrace : Parser[List[Stream[Either[Question,Answer]]]] =
      "<<"~repsep( "["~clientTrace~"]", ":" )~"<<" ^^ {	
	case "<<"~serverTraces~"<<" => {
	  //List[Either[Question,Answer]]( ).toStream
	  serverTraces match {
	    case "["~clientTrace~"]" :: sTraces => {
	      val sTrs =
		( List[Stream[Either[Question,Answer]]]( ) /: sTraces )( 
		  ( acc, e ) => {
		    e match {
		      case "["~clientTrace~"]" => { clientTrace ++ acc }
		    }
		  }
		)
	      List[Stream[Either[Question,Answer]]](
		(
		  List( Left[Question,Answer]( SO ) ).toStream
		  append ( foldTrace( clientTrace ) )
		  append List( Right[Question,Answer]( SC ) ).toStream		  
		)
	      ) ++ sTrs
	    }
	    case Nil => {
	      List[Stream[Either[Question,Answer]]]( )
	    }
	  }
	}	
      }
  }

  def mkClientStream( s : String ) : List[Stream[Either[Question,Answer]]] = {
    val readBack = new StreamMaker
    import readBack._          
    parseAll(
      clientTrace,
      new java.io.StringReader(
	s.replace( "(", ">>(" )
	.replace( ")", ")>>" )
	.replace( "[", "<<[" )
	.replace( "]", "]<<" )
	.replace( "()","(<<<<)" )
	.replace( "[]", "[>>>>]" )
      )
    ) match {
      case Success( r, _ ) => r
      case pE => throw new Exception( "parse error" + pE )
    }
  }

  def mkServerStream( s : String ) : List[Stream[Either[Question,Answer]]] = {
    val readBack = new StreamMaker
    import readBack._
    parseAll(
      serverTrace,
      new java.io.StringReader(
	s.replace( "(", ">>(" )
	.replace( ")", ")>>" )
	.replace( "[", "<<[" )
	.replace( "]", "]<<" )
	.replace( "()","(<<<<)" )
	.replace( "[]", "[>>>>]" )
      )
    ) match {
      case Success( r, _ ) => r
      case pE => throw new Exception( "parse error" + pE )
    }
  }
}

object CSScope extends MonadicGenerators with CSIO with CSData with CSControl {  
  def getSTrace( sstream : QAStream[SO.type,SC.type,CStream] ) : SStream = {
    throw new Exception( "not yet implemented" )
  }

  def getCTrace( sstream : QAStream[CO.type,CC.type,SStream] ) : SStream = {
    throw new Exception( "not yet implemented" )
  }

  // def handleStream( cstream : CStream ) = {
//     for( ( co, sstream, cc ) <- cstream ) yield {
//       shift {
// 	( k : SStream => CC.type ) => {
// 	  k( getSTrace( sstream ) )
// 	}
//       }
//     }
//   }

//   def handleStream( sstream : SStream ) = {
//     for( ( so, sstream, sc ) <- sstream ) yield {
//       shift {
// 	( k : CStream => SC.type ) => {
// 	  k( getCTrace( cstream ) )
// 	}
//       }
//     }
//   }  
}

// C ::= ( S )*

// S ::= [ C ]*


