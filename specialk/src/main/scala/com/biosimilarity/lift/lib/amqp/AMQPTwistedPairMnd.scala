// -*- mode: Scala;-*- 
// Filename:    AMQPTwistedPairMnd.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 10:54:21 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan, _}
import _root_.scala.actors.Actor

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import _root_.java.io.ObjectInputStream
import _root_.java.io.ObjectOutputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.io.ByteArrayOutputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait AMQPTwistedPairScope[T] 
{
  self : AMQPBrokerScope[T] with MonadicDispatcherScope[T] =>

    def src : URI
  def trgt : URI

  def srcHost : String = src.getHost
  def trgtHost : String = trgt.getHost
  def srcPort : Int = src.getPort
  def trgtPort : Int = trgt.getPort

  trait AbstractTwistedQueuePair[T]
  extends AMQPAbstractQueue[T] {
    def srcQ : AMQPAbstractQueue[T]
    def trgtQ : AMQPAbstractQueue[T]
    
    override def exchange : String = {
      if ( srcQ.exchange == trgtQ.exchange ) {
	srcQ.exchange
      }
      else {
	throw new Exception( "src & trgt Q's exchange not equal" )
      }
    }
    override def routingKey : String = {
      if ( srcQ.routingKey == trgtQ.routingKey ) {
	srcQ.routingKey
      }
      else {
	throw new Exception( "src & trgt Q's routingKey not equal" )
      }
    }    
    override def dispatcher : theMDS.Generator[T,Unit,Unit] = {
      srcQ.dispatcher
    }
    override def sender : theMDS.Generator[Unit,T,Unit] = {
      trgtQ.sender
    }    
  }
  
  case class TwistedQueuePair[T](
    override val srcQ : AMQPQueue[T],
    override val trgtQ : AMQPQueue[T]
  ) extends AbstractTwistedQueuePair[T]
  
  class TwistedQueuePairM[A](
    val exchange : String,
    val routingKey : String
  ) extends AMQPQueueMQT[A,TwistedQueuePair] {                
    override def zero [A] : TwistedQueuePair[A] = {
      TwistedQueuePair[A](
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[A]( factory, srcHost, srcPort, exchange ),
	  theMDS.sender[A]( srcHost, srcPort, exchange, routingKey )
	),
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[A]( factory, trgtHost, trgtPort, exchange ),
	  theMDS.sender[A]( trgtHost, trgtPort, exchange, routingKey )
	)
      )
    }    
  }
}

trait JSONOverAMQPTwistedPairScope[T] 
{
  self : JSONOverAMQPBrokerScope[T] with AMQPTwistedPairScope[T] =>     
  class JSONTwistedQueuePairM[A](
    val exchange : String,
    val routingKey : String
  ) extends AMQPQueueMQT[A,TwistedQueuePair]
  with JSONWireToTrgtConversion 
  with AMQPQueueFunctor[String,A,TwistedQueuePair]
 {      
   override type Trgt = A
   override type Wire = String
   override def makeQueue [S] (
     exchange : String,
     routingKey : String,
     dispatcher : theMDS.Generator[S,Unit,Unit],
     sender : theMDS.Generator[Unit,S,Unit]
   ) : TwistedQueuePair[S] = {
     TwistedQueuePair[S](
       new AMQPQueue[S](
	 exchange,
	 routingKey,
	 dispatcher,
	 theMDS.sender[S]( srcHost, srcPort, exchange, routingKey )
       ),
       new AMQPQueue[S](
	 exchange,
	 routingKey,
	 theMDS.serve[S]( factory, trgtHost, trgtPort, exchange ),
	 sender
       )
     )
   }

   def emptyQueue [A] : TwistedQueuePair[String] = {
     map[String,A](
       zero[A],
       ( s : String ) => { wire2Trgt( s ).asInstanceOf[A] },
       ( a : A ) => { trgt2Wire( a.asInstanceOf[Trgt] ) }
     )
   }
    
   override def zero [A] : TwistedQueuePair[A] = {
     makeQueue[A](
       exchange,
       routingKey,
       theMDS.serve[A]( factory, srcHost, srcPort, exchange ),
       theMDS.sender[A]( trgtHost, trgtPort, exchange, routingKey )
     )
   }         
 }
}

class AMQPStdTwistedPairScope[T](
  override val factory : ConnectionFactory,
  override val src : URI,
  override val trgt : URI
) extends AMQPTwistedPairScope[T]
with AMQPBrokerScope[T]
with MonadicDispatcherScope[T] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : AMQPStdTwistedPairScope[T] => {
	(
	  factory.equals( that.factory )
	  && src.equals( that.src )
	  && trgt.equals( that.trgt )
	)
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * factory.hashCode() )
      + ( 37 * src.hashCode() )
      + ( 37 * trgt.hashCode() )
    )
  }
}

object AMQPStdTwistedPairScope {
  def apply [T] (
    factory : ConnectionFactory,
    src : URI,
    trgt : URI
  ) : AMQPStdTwistedPairScope[T] = {
    new AMQPStdTwistedPairScope[T]( factory, src, trgt )
  }
  def unapply [T] (
    amqpStdTPS : AMQPStdTwistedPairScope[T]
  ) : Option[( ConnectionFactory, URI, URI )] = {
    Some( ( amqpStdTPS.factory, amqpStdTPS.src, amqpStdTPS.trgt ) )
  }
}

case class AMQPStdTPS[T](
  override val src : URI,
  override val trgt : URI
) extends AMQPStdTwistedPairScope[T](
  AMQPDefaults.defaultConnectionFactory, src, trgt
)

class StdJSONOverAMQPTwistedPairScope[T](
  override val factory : ConnectionFactory,
  override val src : URI,
  override val trgt : URI
) extends JSONOverAMQPTwistedPairScope[T]
with JSONOverAMQPBrokerScope[T]
with AMQPTwistedPairScope[T] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : StdJSONOverAMQPTwistedPairScope[T] => {
	(
	  factory.equals( that.factory )
	  && src.equals( that.src )
	  && trgt.equals( that.trgt )
	)
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * factory.hashCode() )
      + ( 37 * src.hashCode() )
      + ( 37 * trgt.hashCode() )
    )
  }
}

object StdJSONOverAMQPTwistedPairScope {
  def apply [T] (
    factory : ConnectionFactory,
    src : URI,
    trgt : URI
  ) : StdJSONOverAMQPTwistedPairScope[T] = {
    new StdJSONOverAMQPTwistedPairScope[T]( factory, src, trgt )
  }
  def unapply [T] (
    amqpStdTPS : StdJSONOverAMQPTwistedPairScope[T]
  ) : Option[( ConnectionFactory, URI, URI )] = {
    Some( ( amqpStdTPS.factory, amqpStdTPS.src, amqpStdTPS.trgt ) )
  }
}

case class StdJSONOverAMQPTPS[T](
  override val src : URI,
  override val trgt : URI
) extends StdJSONOverAMQPTwistedPairScope[T](
  AMQPDefaults.defaultConnectionFactory, src, trgt
)

package usage {
  import scala.collection.mutable.HashMap
  import java.util.UUID  

  object AMQPTPSample extends UUIDOps {
    val defaultSrcHost = "10.0.1.5"
    val defaultTrgtHost = "10.0.1.9"

    val defaultQueueUUID = getUUID
    
    val srcSeed = scala.math.round( scala.math.random * 100 ).toInt
    val trgtSeed = scala.math.round( scala.math.random * 100 ).toInt
        
    def setupAndRunTest(
      parity : Boolean,
      srcHost : URI,
      trgtHost : URI,
      queueStr : String,
      useJSON : Boolean,
      msgCount : Int
    ) = {
      val scope = new AMQPStdTPS[Int]( srcHost, trgtHost )

      val qpM =	new scope.TwistedQueuePairM[Int]( queueStr, "routeroute" )

      val qtp = qpM.zero[Int]

      val msgMap = new HashMap[Int,Int]()

      def loop( count : Int ) : Unit = {
	println( "entering msg loop with count : " + count )
	count match {
	  case 0 => {
	    println( "Nothing to do." )
	  }
	  case i => {
	    if ( i < 0 ) {
	      throw new Exception(
		"Tsk, tsk, play fair, now... please keep msg counts positive!"
	      )
	    }
	    else {
	      println(
		"Waiting for a message on queue : " + queueStr
	      )
	      for( msg <- qpM( qtp ) ) {
		val mms = msgMap.size
		println(
		  (
		    "received msg number " + mms
		    + " with contents " + msg
		    + " on " + queueStr 
		  )
		)

		msgMap += ( ( (mms + 1), msg ) )

		if ( mms == count - 1 ) {
		  if ( !parity ) {
		    println(
		      "Ha! We have the last word with msg " + mms
		    )
		    qtp ! mms
		  }
		  println( "All " + count + " messages sent and received." )
		  println( "Conversation summary: " )
		  for( order <- 1 to count ) {
		    val msg = msgMap( order )
		    val prefix = "received " + msg + " "
		    val suffix = 
		      order match {
			case 1 => 1 + "st" + " "
			case 2 => 2 + "nd" + " "
			case 3 => 3 + "rd" + " "
			case _ => order + "th" + " "
		      }
		    println( prefix + suffix + "msg" )
		  }
		  println( "Test successful." )
		}
		else {
		  if ( mms < count ) {
		    println( "replying with msg " + mms )
		    qtp ! mms
		  }
		  else {
		    println( "received unexpected msg: " + msg )
		  }
		}
	      }
	    }
	  }
	}
      }
      
      if ( parity ) {
	println(
	  "sending initial msg " + srcSeed + " on queue " + queueStr
	)
	qtp ! srcSeed
      }

      loop( msgCount )
    }
    
  }

}
