// -*- mode: Scala;-*- 
// Filename:    Monadic.scala 
// Authors:     lgm                                                    
// Creation:    Wed Dec 29 13:57:38 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

//package net.liftweb.amqp
package com.biosimilarity.lift.lib

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client._
import _root_.scala.actors.Actor

import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait MonadicAMQPDispatcher[T]
 extends FJTaskRunners {

  type Ticket = Int

  trait Generable[+A,-B,+C] {
    def funK : (A => (B @suspendable)) => (C @suspendable)
    
    def foreach( f : (A => B @suspendable) ) : C @suspendable = {
      funK( f )
    }
  }

  case class Generator[+A,-B,+C](
    override val funK : (A => (B @suspendable)) => (C @suspendable)
  ) extends Generable[A,B,C] {   
  }
  
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

  def acceptConnections(
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

  def beginService(
    params : ConnectionParameters,
    host : String,
    port : Int
  ) = Generator {
    k : ( T => Unit @suspendable ) =>
      //shift {	
	println( "The rabbit is running... (with apologies to John Updike)" )

	for( channel <- acceptConnections( params, host, port ) ) {
	  spawn {
	    // Open bracket
	    println( "Connected: " + channel )
	    val ticket = channel.accessRequest( "/data" ) 
	    channel.exchangeDeclare( ticket, "mult", "direct" )
	    channel.queueDeclare( ticket, "mult_queue" )
	    channel.queueBind( ticket, "mult_queue", "mult", "routeroute" )
	  
	    for ( t <- readT( channel, ticket ) ) { k( t ) }

	    // println( "Disconnected: " + channel )
	    // Close bracket
	  }
	}
      //}
  }

  case class AMQPDelivery(
    tag   : String,
    env   : Envelope,
    props : AMQP.BasicProperties,
    body  : Array[Byte]
  )

  def callbacks( channel : Channel, ticket : Ticket) =
    Generator {
      k : ( AMQPDelivery => Unit @suspendable) =>

      println("level 1 callbacks")

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
  		   println("before continuation in callback")
  		
    		   k( AMQPDelivery( tag, env, props, body ) )
    		
    		   println("after continuation in callback")
    		   
		   outerk()
    		 }
    	     }
	   }
  	
  	println("before registering callback")
  	
	channel.basicConsume(
	  ticket,
	  "mult_queue",
	  false,
	  TheRendezvous
	)
  	
  	println("after registering callback")
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
  	  
  	      println("readT returning")
  	      outerk()
	    }
	}
    }

}

trait AMQPUtilities {
  def stdCnxnParams : ConnectionParameters = {
    val params = new ConnectionParameters
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
  implicit val defaultConnectionParameters : ConnectionParameters =
    stdCnxnParams
  implicit val defaultHost : String = "localhost"
  implicit val defaultPort : Int = 5672
}

class StdMonadicAMQPDispatcher[T](
  val host : String,
  val port : Int
) extends MonadicAMQPDispatcher[T](
) {
  import AMQPDefaults._

  def acceptConnections()( implicit params : ConnectionParameters )
  : Generator[Channel,Unit,Unit] =
    acceptConnections( params, host, port )
  def beginService()( implicit params : ConnectionParameters )
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
