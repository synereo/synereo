// -*- mode: Scala;-*- 
// Filename:    JSONAMQPClient.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 26 09:43:57 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package net.liftweb.amqp

import _root_.scala.actors.Actor._
import _root_.com.rabbitmq.client._
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ObjectOutputStream

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver
import com.biosimilarity.lift.lib.amqp.RabbitFactory

@transient
class JSONAMQPSender(
  cf: ConnectionFactory,
  host: String,
  port: Int,
  exchange: String,
  routingKey: String
) extends AMQPSender[String](RabbitFactory.getConnection(cf, host, port), exchange, routingKey) {

}

class BasicJSONAMQPSender {
  // All of the params, exchanges, and queues are all just example data.
  val factory = new ConnectionFactory()
  factory.setUsername("guest")
  factory.setPassword("guest")
  factory.setVirtualHost("/")
  factory.setRequestedHeartbeat(0)

  val amqp =
    new JSONAMQPSender(
      factory,
      "localhost",
      5672,
      "mult",
      "routeroute"
    )
  
  def send( contents : java.lang.Object ) : Unit = {
    amqp ! AMQPMessage(
      new XStream( new JettisonMappedXmlDriver() ).toXML( contents )
    )
  }

  amqp.start
  
}

trait JSONAMQPForwarder {
  // All of the params, exchanges, and queues are all just example data.
  val factory = new ConnectionFactory()
  factory.setUsername("guest")
  factory.setPassword("guest")
  factory.setVirtualHost("/")
  factory.setRequestedHeartbeat(0)

  var _amqpSender : Option[JSONAMQPSender] = None

  def amqp( host : String ) = {
    _amqpSender match {
      case Some( amqpSender ) => amqpSender
      case None => {
	val jaS = 
	  new JSONAMQPSender(
	    factory,
	    host,
	    5672,
	    "mult",
	    "routeroute"
	  )
	_amqpSender =
	  Some( jaS )
	jaS.start
	jaS
      }
    }    
  }
  
  def send( contents : java.lang.Object ) : Unit = {
    for( amqp <- _amqpSender ) {
      amqp ! AMQPMessage(
	new XStream( new JettisonMappedXmlDriver() ).toXML( contents )	
      )
    }
  }  
  
}

class StdJSONOverAMQPSender( host : String )
extends JSONAMQPForwarder {
  amqp( host )
}
