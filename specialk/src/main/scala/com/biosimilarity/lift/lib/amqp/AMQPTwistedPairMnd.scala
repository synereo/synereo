// -*- mode: Scala;-*- 
// Filename:    AMQPTwistedPairMnd.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 10:54:21 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.model.msg._

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan, _}

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
        BasicLogService.tweet( "**********************************************************************" )
        BasicLogService.tweet( "in method: exchange" )
        BasicLogService.tweet( "srcQ.exchange: " + srcQ.exchange + " trgtQ.exchange: " + trgtQ.exchange )
        BasicLogService.tweet( "**********************************************************************" )
	throw new Exception( "src & trgt Q's exchange not equal" )
      }
    }
    override def routingKey : String = {
      if ( srcQ.routingKey == trgtQ.routingKey ) {
	srcQ.routingKey
      }
      else {
        BasicLogService.tweet( "**********************************************************************" )
        BasicLogService.tweet( "in method: routingKey" )
        BasicLogService.tweet( "srcQ.routingKey: " + srcQ.routingKey + " trgtQ.routingKey: " + trgtQ.routingKey )
        BasicLogService.tweet( "**********************************************************************" )
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

  class AMQPTwistedPairXForm[W,T](
    override val w2T : W => T,
    override val t2W : T => W,
    val tQP : TwistedQueuePair[W]
  ) extends AMQPQueueXForm[W,T] {
    override def exchange : String = { tQP.exchange }
    override def routingKey : String = { tQP.routingKey }
    override def dispatcherW : theMDS.Generator[W,Unit,Unit] = {
      tQP.dispatcher
    }
    override def senderW : theMDS.Generator[Unit,W,Unit] = {
      tQP.sender
    }

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : AMQPTwistedPairXForm[W,T] => {
	  (
	    w2T.equals( that.w2T )
	    && t2W.equals( that.t2W )
	    && tQP.equals( that.tQP )
	  )
	}
	case _ => false
      }
    }
    override def hashCode( ) : Int = {
      (	
	( 37 * w2T.hashCode )
	+ ( 37 * t2W.hashCode )
	+ ( 37 * tQP.hashCode )
      )
    }
  }  
  
  object AMQPTwistedPairXForm {
    def apply [W,T] (
      w2T : W => T, t2W : T => W, tQP : TwistedQueuePair[W]
    ) : AMQPTwistedPairXForm[W,T] = {
      new AMQPTwistedPairXForm[W,T]( w2T, t2W, tQP )
    }
    def unapply [W,T] (
      amqpTPXfm : AMQPTwistedPairXForm[W,T]
    ) : Option[( W => T, T => W, TwistedQueuePair[W] )] = {
      Some(
	( amqpTPXfm.w2T, amqpTPXfm.t2W, amqpTPXfm.tQP )
      )
    }
  }
  
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

    case class AMQPTwistedPairJSONXForm[A](
      override val w2T : String => A,
      override val t2W : A => String,
      override val tQP : TwistedQueuePair[String]
    ) extends AMQPTwistedPairXForm[String,A]( w2T, t2W, tQP )

  class JSONOverAMQPTwistedPairXFormM[A](
    override val exchange : String,
    override val routingKey : String
  ) extends AMQPQueueMQT[A,AMQPTwistedPairJSONXForm] {
    override def zero [A] : AMQPTwistedPairJSONXForm[A] = {
      val a2S : A => String = 
	( a : A ) => {
	  new XStream( new JettisonMappedXmlDriver ).toXML( a )
	}
      val s2A : String => A = 
	( s : String ) => {
	  new XStream(
	    new JettisonMappedXmlDriver
	  ).fromXML( s ).asInstanceOf[A]
	}
      AMQPTwistedPairJSONXForm[A](
	s2A,
	a2S,
	TwistedQueuePair[String](
	  AMQPQueue[String](
	    exchange,
	    routingKey,
	    theMDS.serve[String]( factory, srcHost, srcPort, exchange ),
	    theMDS.sender[String]( srcHost, srcPort, exchange, routingKey )
	  ),
	  AMQPQueue[String](
	    exchange,
	    routingKey,
	    theMDS.serve[String]( factory, trgtHost, trgtPort, exchange ),
	    theMDS.sender[String]( trgtHost, trgtPort, exchange, routingKey )
	  )
	)
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

trait JustificationOverAMQPTwistedPairScope[Req,Rsp] 
{
  self : AMQPTwistedPairScope[Either[Req,Rsp]] with AMQPBrokerScope[Either[Req,Rsp]] with MonadicDispatcherScope[Either[Req,Rsp]] =>     

  case class AMQPTwistedPairJustificationXForm[T](
    override val w2T : Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]] => T,
    override val t2W : T => Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]],
    override val tQP : TwistedQueuePair[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]]
  ) extends
    AMQPTwistedPairXForm[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]],T]( w2T, t2W, tQP )

  class JustificationOverAMQPTwistedPairXFormM[A](
    override val exchange : String,
    override val routingKey : String
  ) extends AMQPQueueMQT[A,AMQPTwistedPairJustificationXForm] with UUIDOps {
    override def zero [A] : AMQPTwistedPairJustificationXForm[A] = {
      val a2S : A => Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]] = 
	( a : A ) => {
	  a match {
	    case Left( req : Req ) => {
	      Left[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]](
		JustifiedRequest[Req,Rsp](
		  getUUID(),
		  MURI( trgt ),
		  MURI( src ),
		  getUUID(),
		  req,
		  None
		)
	      )
	    }
	    case Right( rsp : Rsp ) => {
	      Right[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]](
		JustifiedResponse[Req,Rsp](
		  getUUID(),
		  MURI( trgt ),
		  MURI( src ),
		  getUUID(),
		  rsp,
		  None
		)
	      )
	    }
	  }
	}
      val s2A : Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]] => A = 
	( s : Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]] ) => {
	  s match {
	    case Left( JustifiedRequest( _, _, _, _, req : Req, _ ) ) => {
	      Left[Req,Rsp]( req ).asInstanceOf[A]
	    }
	    case Right( JustifiedResponse( _, _, _, _, rsp : Rsp, _ ) ) => {
	      Right[Req,Rsp]( rsp ).asInstanceOf[A]
	    }
	  }
	}
      AMQPTwistedPairJustificationXForm[A](
	s2A,
	a2S,
	TwistedQueuePair[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]](
	  AMQPQueue[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]](
	    exchange,
	    routingKey,
	    theMDS.serve[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]]( factory, srcHost, srcPort, exchange ),
	    theMDS.sender[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]]( srcHost, srcPort, exchange, routingKey )
	  ),
	  AMQPQueue[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]](
	    exchange,
	    routingKey,
	    theMDS.serve[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]]( factory, trgtHost, trgtPort, exchange ),
	    theMDS.sender[Either[JustifiedRequest[Req,Rsp],JustifiedResponse[Req,Rsp]]]( trgtHost, trgtPort, exchange, routingKey )
	  )
	)
      )
    }    
  }
}

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
      val scope :
	    AMQPTwistedPairScope[Int]
	     with AMQPBrokerScope[Int]
	     with MonadicDispatcherScope[Int] =
	       if ( useJSON ) {
		 StdJSONOverAMQPTPS[Int]( srcHost, trgtHost )
	       }
	       else {
		 AMQPStdTPS[Int]( srcHost, trgtHost )   
	       }

      val qpM : scope.AMQPQueueMQT[Int,scope.AMQPAbstractQueue] =
	(if ( useJSON ) {
	  val s : StdJSONOverAMQPTPS[Int] =
	    scope.asInstanceOf[StdJSONOverAMQPTPS[Int]]
	  new s.JSONOverAMQPTwistedPairXFormM[Int]( queueStr, "routeroute" )
	}
	 else {
	  new scope.TwistedQueuePairM[Int]( queueStr, "routeroute" ) 
	 }).asInstanceOf[scope.AMQPQueueMQT[Int,scope.AMQPAbstractQueue]]
      val qtp : scope.AMQPAbstractQueue[Int] = qpM.zero[Int]

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
  
