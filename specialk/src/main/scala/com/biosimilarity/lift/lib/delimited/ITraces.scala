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

  case class GameState(
    history : GameHistory,
    oCK : Option[GameState => GameState],
    oSK : Option[GameState => GameState]
  )

  def handleClientStream(
    history : GameHistory,
    oCK : Option[GameState => GameState],
    oSK : Option[GameState => GameState]
  )( loop : Boolean ) : GameState = {
    println( "handling client stream: " + history + " , " + oSK + " , " + oCK )

    val ghist = history.drop( 1 ).asInstanceOf[GameHistory]

    reset {
      history.take( 1 )( 0 ) match {
	case Left( qn ) => {
	  println( "handling: " + qn )
	  qn match {
	    case CO => {
	      //print( CO )
	      val gs =	      
		shift {
		  ( k : GameState => GameState ) => {		  
		    println( "co-recursively calling handleServerStream: " + ghist + " , " + Some( k ) + " , " + oSK )
		    handleServerStream( ghist, Some( k ), oSK )( loop )		  
		  }
		}	      

	      val ngs =
		if ( loop ) {
		  println( "looping on server stream: " + gs.history + " , " + oCK + " , " + oSK )
		  if ( !gs.history.isEmpty ) {
		    handleServerStream( gs.history, gs.oCK, oSK )( loop )
		  }
		  else {
		    gs
		  }
		} else {
		  gs
		}	    
  	      
	      oSK match {
		case Some( sK ) => {		      
		  println( "calling server continuation (1): " + ngs.history )
		    sK( ngs )
		}
		case None => {
		  ngs
		}
	      }
	    }
	    case SO => {
	      throw new Exception( "unexpected move in handleClientStream: " + SO + " , " + ghist + " , " + oCK + " , " + oSK )
	    }
	  }
	}
	case Right( ans ) => {
	  println( "handling: " + ans )
	  ans match {
	    case CC => {
	      oSK match {
		case Some( sK ) => {
		  //print( SC )
		  println( "calling server continuation (2): " + ghist )
		  sK( GameState( ghist, oCK, oSK ) )
		}
		case None => {
		  GameState( ghist, oCK, oSK )
		}
	      }
	    }
	    case SC => {
	      oCK match {
		case Some( cK ) => {
		  println( "calling client continuation (2): " + ghist )
		  cK( GameState( ghist, oCK, oSK ) )
		}
		case None => {
		  oSK match {
		    case Some( sK ) => {
		      sK( GameState( ghist, None, None ) )
		    }
		    case None => {
		      GameState( ghist, oCK, oSK )
		    }
		  }		  
		}
	      }
	    }
	  }
	}
      }
    }
  }

  def handleServerStream( 
    history : GameHistory,
    oCK : Option[GameState => GameState],
    oSK : Option[GameState => GameState]
  )( loop : Boolean ) : GameState = {
    println( "handling server stream: " + history + " , " + oCK + " , " + oSK )

    val ghist = history.drop( 1 ).asInstanceOf[GameHistory]

    reset {
      history.take( 1 )( 0 ) match {
	case Left( qn ) => {
	  println( "handling: " + qn )
	  qn match {
	    case SO => {
	      //print( SO )
	      val gs =
		shift {
		  ( k : GameState => GameState ) => {
		    println( "co-recursively calling handleClientStream: " + ghist + " , " + oCK + " , " + Some( k ) )
		    handleClientStream( ghist, oCK, Some( k ) )( loop )		      		  		  
		  }
		}

	      val ngs =
		if ( loop ) {
		  println( "looping on client stream: " + gs.history + " , " + oCK + " , " + oSK )
		  if ( !gs.history.isEmpty ) {
		    handleClientStream( gs.history, oCK, gs.oSK )( loop )
		  }
		  else {
		    gs
		  }
		} else {
		  gs
		}
	      
	      oCK match {
		case Some( cK ) => {		      
		  println( "calling client continuation (1): " + ngs.history )
		  cK( ngs )
		}
		case None => {
		  ngs
		}
	      }
	    }
	    case CO => {
	      throw new Exception( "unexpected move in handleServerStream: " + CO + " , " + ghist + " , " + oCK + " , " + oSK )
	    }
	  }
	}
	case Right( ans ) => {
	  println( "handling: " + ans )
	  ans match {
	    case SC => {
	      oCK match {
		case Some( cK ) => {
		  //print( CC )
		  println( "calling client continuation (2): " + ghist )
		  cK( GameState( ghist, oCK, oSK ) )
		}
		case None => {
		  GameState( ghist, oCK, oSK )
		}
	      }
	    }
	    case CC => {
	      oSK match {
		case Some( sK ) => {
		  println( "calling server continuation (2): " + ghist )
		  sK( GameState( ghist, oCK, oSK ) )
		}
		case None => {
		  oCK match {
		    case Some( cK ) => {
		      println( "calling client continuation (3): " + ghist )
		      cK( GameState( ghist, None, None ) )
		    }
		    case None => {
		      GameState( ghist, oCK, oSK )
		    }
		  }		  
		}
	      }
	    }
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
      repsep( "("~serverTrace~")", ":" ) ^^ {
	case "("~serverTrace~")" :: cTraces => {
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>> clientTrace >>>>>>>>>>>>>>>>>>>>>>>>>>" )
	  println( "parsed server trace: " + serverTrace )
	  println( "remaining client traces: " + serverTrace )

	  val sTr = foldTrace( serverTrace )
	  val cTr =
	    List[Stream[Either[Question,Answer]]](
	      (
		List( Left[Question,Answer]( CO ) ).toStream
		append sTr
		append List( Right[Question,Answer]( CC ) ).toStream		  
	      )
	    ) 
	  val cTrs =
	    ( List[Stream[Either[Question,Answer]]]( ) /: cTraces )( 
	      ( acc, e ) => {
		e match {
		  case "("~serverTrace~")" => {
		    val sTr = foldTrace( serverTrace )
		    val cTr =
		      List[Stream[Either[Question,Answer]]](
			(
			  List( Left[Question,Answer]( CO ) ).toStream
			  append sTr
			  append List( Right[Question,Answer]( CC ) ).toStream		  
			)
		      ) 

		    cTr ++ acc
		  }
		}
	      }
	    )

	  println( "folded server trace: " + sTr )
	  println( "client trace: " + cTr )
	  println( "folded client traces: " + cTrs )
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>> clientTrace >>>>>>>>>>>>>>>>>>>>>>>>>>" )
	  
	  cTr ++ cTrs
	}
	case Nil => {
	  List[Stream[Either[Question,Answer]]]( )
	}	
      }

    def serverTrace : Parser[List[Stream[Either[Question,Answer]]]] =
      repsep( "["~clientTrace~"]", ":" ) ^^ {	
	case "["~clientTrace~"]" :: sTraces => {
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>> serverTrace >>>>>>>>>>>>>>>>>>>>>>>>>>" )
	  println( "parsed client trace: " + clientTrace )
	  println( "remaining server traces: " + sTraces )
	  
	  val cTr = foldTrace( clientTrace )
	  val sTr =
	    List[Stream[Either[Question,Answer]]](
	      (
		List( Left[Question,Answer]( SO ) ).toStream
		append cTr 
		append List( Right[Question,Answer]( SC ) ).toStream		  
	      )
	    )
	  val sTrs =
	    ( List[Stream[Either[Question,Answer]]]( ) /: sTraces )( 
	      ( acc, e ) => {
		e match {
		  case "["~clientTrace~"]" => {
		    val cTr = foldTrace( clientTrace )
		    val sTr =
		      List[Stream[Either[Question,Answer]]](
			(
			  List( Left[Question,Answer]( SO ) ).toStream
			  append cTr 
			  append List( Right[Question,Answer]( SC ) ).toStream		  
			)
		      )

		    sTr ++ acc
		  }
		}
	      }
	    )	  

	  println( "folded client trace: " + cTr )
	  println( "server trace: " + sTr )
	  println( "folded server traces: " + sTrs )
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>> serverTrace >>>>>>>>>>>>>>>>>>>>>>>>>>" )

	  sTr ++ sTrs
	}
	case Nil => {
	  List[Stream[Either[Question,Answer]]]( )
	}	
      }
  }

  def mkClientStream( s : String ) : List[Stream[Either[Question,Answer]]] = {
    val readBack = new StreamMaker
    import readBack._          
    parseAll(
      clientTrace,
      new java.io.StringReader(
	s /* .replace( "(", ">>(" )
	.replace( ")", ")>>" )
	.replace( "[", "<<[" )
	.replace( "]", "]<<" )
	.replace( "()","(<<<<)" )
	.replace( "[]", "[>>>>]" ) */
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
	s /* .replace( "(", ">>(" )
	.replace( ")", ")>>" )
	.replace( "[", "<<[" )
	.replace( "]", "]<<" )
	.replace( "()","(<<<<)" )
	.replace( "[]", "[>>>>]" ) */
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


