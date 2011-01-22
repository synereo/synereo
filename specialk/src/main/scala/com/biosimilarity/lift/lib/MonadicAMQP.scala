// -*- mode: Scala;-*- 
// Filename:    MonadicAMQP.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 13:10:54 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan,
				   ConnectionParameters => RabbitCnxnParams, _}
import _root_.scala.actors.Actor

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait MonadicAMQPDispatcher[T]
 extends MonadicDispatcher[T] {
   self : WireTap with Journalist =>

  case class AMQPDelivery(
    tag   : String,
    env   : Envelope,
    props : AMQP.BasicProperties,
    body  : Array[Byte]
  )

  type ConnectionParameters = RabbitCnxnParams
  type Channel = RabbitChan
  type Ticket = Int
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
    params : ConnectionParameters,
    host : String,
    port : Int
  ) =
    Generator {
      k : ( Channel => Unit @suspendable ) => {
	//shift {
	  //innerk : (Unit => Unit @suspendable) => {
	    val factory = new ConnectionFactory( params )
	    val connection = factory.newConnection( host, port )
	    val channel = connection.createChannel()
	    k( channel );
	  //}
	//}      
      }
    }

  override def beginService(
    params : ConnectionParameters,
    host : String,
    port : Int
  ) = Generator {
    k : ( T => Unit @suspendable ) =>
      //shift {	
	reportage(
	  "The rabbit is running... (with apologies to John Updike)"
	)

	for( channel <- acceptConnections( params, host, port ) ) {
	  spawn {
	    // Open bracket
	    reportage( "Connected: " + channel )
	    val ticket = channel.accessRequest( "/data" ) 
	    channel.exchangeDeclare( ticket, "mult", "direct" )
	    channel.queueDeclare( ticket, "mult_queue" )
	    channel.queueBind( ticket, "mult_queue", "mult", "routeroute" )
	  
	    for ( t <- readT( channel, ticket ) ) { k( t ) }

	    // reportage( "Disconnected: " + channel )
	    // Close bracket
	  }
	}
      //}
  }

  override def callbacks( channel : Channel, ticket : Ticket) =
    Generator {
      k : ( Payload => Unit @suspendable) =>

      reportage("level 1 callbacks")

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
  		   reportage("before continuation in callback")
  		
    		   k( AMQPDelivery( tag, env, props, body ) )
    		
    		   reportage("after continuation in callback")
    		   
		   outerk()
    		 }
    	     }
	   }
  	
  	reportage("before registering callback")
  	
	channel.basicConsume(
	  ticket,
	  "mult_queue",
	  false,
	  TheRendezvous
	)
  	
  	reportage("after registering callback")
  	// stop
      }
    }

  def readT( channel : Channel, ticket : Ticket ) =
    Generator {
      k: ( T => Unit @suspendable) =>
	shift {
	  outerk: (Unit => Unit) =>
	    reset {
	      
  	      for (
		amqpD <- callbacks( channel, ticket )
	      )	{
  		val routingKey = amqpD.env.getRoutingKey
		val contentType = amqpD.props.contentType
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
  	  
  	      reportage( "readT returning" )
  	      outerk()
	    }
	}
    }

}

trait AMQPUtilities {
  def stdCnxnParams : RabbitCnxnParams = {
    val params = new RabbitCnxnParams //new ConnectionParameters
    params.setUsername( "guest" )
    params.setPassword( "guest" )
    params.setVirtualHost( "/" )
    params.setRequestedHeartbeat( 0 )
    params
  }
}

object AMQPDefaults extends AMQPUtilities {
  implicit val defaultConnectionFactory : ConnectionFactory =
    new ConnectionFactory( defaultConnectionParameters )
  implicit val defaultConnectionParameters : RabbitCnxnParams =
    stdCnxnParams
  implicit val defaultHost : String = "localhost"
  implicit val defaultPort : Int = 5672
  implicit val defaultDispatching : Boolean = true
}

class StdMonadicAMQPDispatcher[T](
  val host : String,
  val port : Int
) extends MonadicAMQPDispatcher[T](
) with WireTap with Journalist {
  import AMQPDefaults._

  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }

  def acceptConnections()( implicit params : RabbitCnxnParams )
  : Generator[Channel,Unit,Unit] =
    acceptConnections( params, host, port )
  def beginService()( implicit params : RabbitCnxnParams )
  : Generator[T,Unit,Unit] =
    beginService( params, host, port )
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
extends MonadicAMQPDispatcher[String] {
  self : WireTap with Journalist =>
    import AMQPDefaults._
  
  def srcURI : URI
  def trgtURI : URI

  var _jsonDispatcher : Option[StdMonadicAMQPDispatcher[String]] = None
  def jsonDispatcher( handle : T => Unit )(
    implicit dispatchOnCreate : Boolean, port : Int
  ) : StdMonadicAMQPDispatcher[String] = {
    _jsonDispatcher match {
      case Some( jd ) => jd
      case None => {
	val jd =
	  new StdMonadicAMQPDispatcher[String]( srcURI.getHost, port )

	if ( dispatchOnCreate ) {
	  val xstrm = new XStream( new JettisonMappedXmlDriver )
	  reset {
	    for( msg <- jd.beginService() ){
	      val msgT : T = xstrm.fromXML( msg ).asInstanceOf[T]
	      handle( msgT )
	    }
	  }	
	}

	_jsonDispatcher = Some( jd )
	jd
      }
    }
  }

  var _jsonSender : Option[JSONAMQPSender] = None 
  
  def jsonSender()( implicit params : RabbitCnxnParams, port : Int ) : JSONAMQPSender = {
    _jsonSender match {
      case Some( js ) => js
      case None => {
	val js = new JSONAMQPSender(
	  new ConnectionFactory( params ),
	  trgtURI.getHost,
	  port,
	  "mult",
	  "routeroute"
	)       

	_jsonSender = Some( js )

	js.start
	js
      }
    }
  }
}

class SMJATwistedPair[T](
  override val srcURI : URI,
  override val trgtURI : URI
) extends SemiMonadicJSONAMQPTwistedPair[T]
with WireTap
with Journalist {
  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }
}

object SMJATwistedPair {
  def apply[T] (
    srcIPStr : String, trgtIPStr : String
  ) : SMJATwistedPair[T] = {
    new SMJATwistedPair[T](
      new URI( "agent", srcIPStr, "/invitation", "" ),
      new URI( "agent", trgtIPStr, "/invitation", "" )
    )
  }
  def unapply[T](
    smjatp : SMJATwistedPair[T]
  ) : Option[(URI,URI)] = {
    Some( ( smjatp.srcURI, smjatp.trgtURI ) )
  }    
}
