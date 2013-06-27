// -*- mode: Scala;-*- 
// Filename:    rabbit-connectivity.scala 
// Authors:     lgm                                                    
// Creation:    Tue Apr 19 13:10:19 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.usage._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.usage._
import scala.util.continuations._ 

object RabbitMQConnectivityTest extends UUIDOps {
  import AMQPDefaults._
  import MonadicAMQPUnitTest._
  import scala.collection.mutable.HashMap

  def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
    lazy val loopStrm : Stream[T] =
      ( List( seed ) ).toStream append ( loopStrm map fresh );
    loopStrm
  }

  case class Msg( b : Boolean, s : String, i : Int, r : Option[Msg] )

  def msgStrm() : Stream[Msg] = {
    tStream[Msg]( new Msg( true, "yo!", 0, None ) )(
      {
	( m : Msg ) => {
	  new Msg( m.b, m.s, m.i + 1, Some( m ) )
	}
      }
    )
  }

  def freshQueueName() : String = {
    "amqp_" + getUUID
  }
  def setupConnectionApparatus(
    queueName : String,
    a : String,
    b : String,
    msgCount : Int,
    msgCx : HashMap[String,List[Msg]]
  ) = {
    val sma1 = SMJATwistedPair[Msg]( a, b )
    (
      sma1,
      sma1.jsonSender( queueName ),
      sma1.jsonDispatcher(
	queueName,
	( x ) => {
	  println( "received : " + x )
	  val msgs = msgCx.getOrElse( queueName, Nil )
	  msgCx += ( ( queueName, msgs ++ List( x ) ) )
	}
      ),
      msgStrm.take( msgCount )
    )
  }

  def checkConnectivity( a : String, b : String ) = {
    val queueName = freshQueueName
    val msgCx = new HashMap[String,List[Msg]]()
    def msgCount = {
      msgCx.getOrElse( queueName, Nil ).size
    }
    val ( sma1, jsdnr, jsdsp, msgs ) =
      setupConnectionApparatus(
	queueName,
	a, b, 100,
	msgCx
      )
    
    for( i <- 0 to 9 ) { sma1.send( msgs( i ) ) }
      
    while ( msgCount < 10 ) { Thread.sleep( 100 ) }

    ( queueName, msgCx )
  }      
}
