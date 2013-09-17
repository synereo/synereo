// -*- mode: Scala;-*- 
// Filename:    MonadicAMQP.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 13:10:54 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import amqp.RabbitFactory
import com.biosimilarity.lift.lib.moniker._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan, _}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait MonadicAMQPDispatcher[T]
 extends MonadicDispatcher[T] with Serializable {
   self : WireTap =>

  case class AMQPDelivery(
    tag   : String,
    env   : Envelope,
    props : AMQP.BasicProperties,
    body  : Array[Byte]
  )

  type Channel = RabbitChan
  type Payload = AMQPDelivery

  abstract class SerializedConsumer[T](
    val channel : Channel
  ) extends DefaultConsumer( channel ) {
    override def handleDelivery(
      tag : String,
      env : Envelope,
      props : AMQP.BasicProperties,
      body : Array[Byte]
    )
  }  

  override def acceptConnections(
    factory : ConnectionFactory,
    host : String,
    port : Int
  ) =
    Generator {
      k : ( Channel => Unit @suspendable ) => {
	//shift {
	  //innerk : (Unit => Unit @suspendable) => {

	    val connection =
              RabbitFactory.getConnection(factory, host, port)
	    val channel =
	      connection.createChannel()
	    k( channel );
	  //}
	//}      
      }
    }   

  override def beginService(
    factory : ConnectionFactory,
    host : String,
    port : Int
  ) = {
    beginService( factory, host, port, "mult" )
  }

   def beginService(
    factory : ConnectionFactory,
    host : String,
    port : Int,
    exQNameRoot : String
  ) = serve [T] ( factory, host, port, exQNameRoot )

   def serve [T] (
    factory : ConnectionFactory,
    host : String,
    port : Int,
    exQNameRoot : String
  ) = Generator {
    k : ( T => Unit @suspendable ) =>
      //shift {
	BasicLogService.blog(
	  "The rabbit is running... (with apologies to John Updike)"
	)

	for( channel <- acceptConnections( factory, host, port ) ) {
	  spawn {
	    // Open bracket
	    BasicLogService.blog( "Connected: " + channel )
            val qname = (exQNameRoot + "_queue")
            channel.exchangeDeclare( exQNameRoot, "direct" )
            channel.queueDeclare(qname, true, false, false, null);
            channel.queueBind( qname, exQNameRoot, "routeroute" )

            for ( t <- read [T] ( channel, exQNameRoot ) ) { k( t ) }

            // Close bracket
	  }
	}
      //}
  }

  override def callbacks( channel : Channel) = {
    callbacks( channel, "mult" )
  }

  def callbacks(
    channel : Channel, exQNameRoot : String
  ) =
    Generator {
      k : ( Payload => Unit @suspendable) =>

      BasicLogService.blog("level 1 callbacks")

      shift {
	outerk : (Unit => Any) =>
	  
	  object TheRendezvous
	   extends SerializedConsumer[T]( channel ) {
    	     override def handleDelivery(
	       tag : String,
	       env : Envelope,
	       props : AMQP.BasicProperties,
	       body : Array[Byte]
	     ) {
    		 spawn { 
  		   BasicLogService.blog("before continuation in callback")
  		
    		   k( AMQPDelivery( tag, env, props, body ) )
    		
    		   BasicLogService.blog("after continuation in callback")
    		   
		   outerk()
    		 }
    	     }
	   }
  	
  	BasicLogService.blog("before registering callback")
  	
	channel.basicConsume(
//	  ticket,
	  exQNameRoot + "_queue",
	  false,
	  TheRendezvous
	)
  	
  	BasicLogService.blog("after registering callback")
  	// stop
      }
    }

   def readT( channel : Channel ) = {
     readT( channel, "mult" )
   }   

   def readT( channel : Channel, exQNameRoot : String ) =
     read [T] ( channel, exQNameRoot )

   def read [T] ( channel : Channel, exQNameRoot : String ) =
     Generator {
       k: ( T => Unit @suspendable) =>
	 shift {
	   outerk: (Unit => Unit) =>
	     reset {
	      
  	       for (
		 amqpD <- callbacks( channel, exQNameRoot )
	       )	{
  		 val routingKey = amqpD.env.getRoutingKey
		 val contentType = amqpD.props.getContentType
		 val deliveryTag = amqpD.env.getDeliveryTag
		 val in =
		   new ObjectInputStream(
		     new ByteArrayInputStream( amqpD.body )
		   )
		 val t = in.readObject.asInstanceOf[T];
		 k( t )
		 channel.basicAck(deliveryTag, false);
		 
		 // Is this necessary?
		 shift { k : ( Unit => Unit ) => k() }
  	       }
  	       
  	       BasicLogService.blog( "readT returning" )
  	       outerk()
	     }
	 }
     }
   
 }

trait JSONWireToTrgtConversion
extends WireToTrgtConversion {
  override type Wire = String

  override def wire2Trgt( wire : Wire ) : Trgt = {
    val xstrm = new XStream( new JettisonMappedXmlDriver )
    xstrm.fromXML( wire ).asInstanceOf[Trgt]
  }
  override def trgt2Wire( trgt : Trgt ) : Wire = {
    val xstrm = new XStream( new JettisonMappedXmlDriver )
    xstrm.toXML( trgt ).asInstanceOf[Wire]
  }
}

trait MonadicJSONAMQPDispatcher[T]
extends JSONWireToTrgtConversion
{
  self : MonadicWireToTrgtConversion with WireTap =>
    type Trgt = T   
}

trait DefaultMonadicAMQPDispatcher[T]
extends MonadicAMQPDispatcher[T] {
  self : WireTap =>
  //import AMQPDefaults._
  
  def host : String
  def port : Int

  override def tap [A] ( fact : A ) : Unit = {
    BasicLogService.reportage( fact )
  }

  def acceptConnections()( implicit connectionFactory : ConnectionFactory )
  : Generator[Channel,Unit,Unit] =
    acceptConnections( connectionFactory, host, port )
  def beginService()( implicit connectionFactory : ConnectionFactory )
  : Generator[T,Unit,Unit] =
    beginService( connectionFactory, host, port )
  def beginService( exQNameStr : String )( implicit connectionFactory : ConnectionFactory )
  : Generator[T,Unit,Unit] =
    beginService( connectionFactory, host, port, exQNameStr )
}

class StdMonadicAMQPDispatcher[T](
  override val host : String,
  override val port : Int
) extends DefaultMonadicAMQPDispatcher[T](
) with WireTap {
}

class StdMonadicJSONAMQPDispatcher[T](
  override val host : String,
  override val port : Int
) extends StdMonadicAMQPDispatcher[String]( host, port )
with MonadicJSONAMQPDispatcher[T]
with MonadicWireToTrgtConversion {
}

object StdMonadicAMQPDispatcher {
  def apply[T] (
    host : String, port : Int
  ) : StdMonadicAMQPDispatcher[T] = {
    new StdMonadicAMQPDispatcher(
      host, port
    )
  }
  def unapply[T](
    smAMQPD : StdMonadicAMQPDispatcher[T]
  ) : Option[(String,Int)] = {
    Some( ( smAMQPD.host, smAMQPD.port ) )
  }    
}

trait SemiMonadicJSONAMQPTwistedPair[T]
{  
  self : WireTap =>

  import AMQPDefaults._
  
  def srcURI : Moniker
  def trgtURI : Moniker

  def getPort(uriPort: Int, defaultPort: Int): Int = {
    uriPort match {
      case -1 => defaultPort
      case _ => uriPort
    }
  }

  @transient var _jsonDispatcher : Option[StdMonadicJSONAMQPDispatcher[T]] = None
  def jsonDispatcher( handle : T => Unit )(
    implicit dispatchOnCreate : Boolean, port : Int
  ) : StdMonadicJSONAMQPDispatcher[T] = {
    jsonDispatcher( "mult", handle )( dispatchOnCreate, port )
  }

  def jsonDispatcher( exQNameStr : String, handle : T => Unit )(
    implicit dispatchOnCreate : Boolean, port : Int
  ) : StdMonadicJSONAMQPDispatcher[T] = {
    _jsonDispatcher match {
      case Some( jd ) => jd
      case None => {
	val jd =
	  new StdMonadicJSONAMQPDispatcher[T]( srcURI.getHost, getPort(srcURI.getPort, port) )

	if ( dispatchOnCreate ) {
	  reset {
	    for(
	      msg <- jd.xformAndDispatch(
		jd.beginService( exQNameStr )
	      )
	    ) {
	      handle( msg )
	    }
	  }	
	}

	_jsonDispatcher = Some( jd )
	jd
      }
    }
  }

  @transient var _jsonSender : Option[JSONAMQPSender] = None 
  
  def jsonSender()( implicit connectionFactory : ConnectionFactory, defaultPort : Int ) : JSONAMQPSender = {
    jsonSender( "mult" )( connectionFactory, defaultPort )
  }

  def jsonSender( exchNameStr : String )( implicit connectionFactory : ConnectionFactory, port : Int ) : JSONAMQPSender = {
    _jsonSender match {
      case Some( js ) => js
      case None => {
	val js = new JSONAMQPSender(
	  connectionFactory,
	  trgtURI.getHost,
	  getPort(trgtURI.getPort, port),
	  exchNameStr,
	  "routeroute"
	)       

	_jsonSender = Some( js )

	js.start
	js
      }
    }
  }

  def send( contents : T ) : Unit = {
    for( amqp <- _jsonSender ) {
      val body =
	new XStream(
	  new JettisonMappedXmlDriver()
	).toXML( contents )	

      // BasicLogService.tweet(
// 	(
// 	  this 
// 	  + " is sending "
// 	  + contents
// 	  + " encoded as "
// 	  + body
// 	  + " along "
// 	  + amqp
// 	)
//       )
      amqp ! AMQPMessage( body )
    }
  }  
}

class SMJATwistedPair[T](
  override val srcURI : Moniker,
  override val trgtURI : Moniker
) extends SemiMonadicJSONAMQPTwistedPair[T]
  with WireTap {
    override def tap [A] ( fact : A ) : Unit = {
      BasicLogService.reportage( fact )
    }
}

object SMJATwistedPair {  
  def apply[T] (
    srcURI : Moniker, trgtURI : Moniker
  ) : SMJATwistedPair[T] = {
    new SMJATwistedPair[T](
         srcURI,
         trgtURI
    )
  }

  def apply[T] (
    srcIPStr : String, trgtIPStr : String
  ) : SMJATwistedPair[T] = {
    new SMJATwistedPair[T](
      MURI( new URI( "agent", srcIPStr, "/invitation", "" ) ),
      MURI( new URI( "agent", trgtIPStr, "/invitation", "" ) )
    )
  }
  def unapply[T](
    smjatp : SMJATwistedPair[T]
  ) : Option[(Moniker,Moniker)] = {
    Some( ( smjatp.srcURI, smjatp.trgtURI ) )
  }    
}

package usage {
  import com.biosimilarity.lift.lib.amqp.utilities._
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

object MonadicAMQPUnitTest
extends AMQPTestUtility[String] {
  import AMQPDefaults._  
  
  override def msgStreamPayload( idx : Int ) : String = { "Msg" + idx }  

  val srcIPStr = "10.0.1.5"
  val trgtIPStr = "10.0.1.9"
  
  trait Destination
  case class Src() extends Destination
  case class Trgt() extends Destination
  def smjatp( d : Destination ) = {
    val _smjatp =
      d match {
	case Src() => {
	  SMJATwistedPair[Msg]( srcIPStr, trgtIPStr )
	}
	case Trgt() => {
	  SMJATwistedPair[Msg]( trgtIPStr, srcIPStr )
	}
      }
    _smjatp.jsonDispatcher(
      ( msg ) => { println( "received : " + msg ) }
    )
    val msgs = msgStream.take( 100 ).toList
    _smjatp.jsonSender
    // for( i <- 1 to 100 ) {
//       _smjatp.send( msgs( i - 1 ) )
//     }
    _smjatp
  }
}

}
