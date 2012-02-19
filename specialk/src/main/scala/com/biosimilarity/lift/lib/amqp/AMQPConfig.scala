// -*- mode: Scala;-*- 
// Filename:    AMQPConfig.scala 
// Authors:     lgm                                                    
// Creation:    Sun Feb 19 10:51:46 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.lib.moniker._

import net.liftweb.amqp._

import com.rabbitmq.client.{ Channel => RabbitChan, _}

import java.net.URI

trait AMQPUtilities {
}

object AMQPDefaults extends AMQPUtilities {
  def getDefaultConnectionFactory(): ConnectionFactory =
  {
    val factory = new ConnectionFactory(  )
    factory.setUsername("guest")
    factory.setPassword("guest")
    factory.setVirtualHost("/")
    factory.setRequestedHeartbeat(0)
    factory
  }

  implicit val defaultConnectionFactory : ConnectionFactory = getDefaultConnectionFactory()
  implicit val defaultHost : String = "localhost"
  implicit val defaultPort : Int = 5672  
  implicit val defaultDispatching : Boolean = true
  val defaultExchange : String = "mult"
  val defaultRoutingKey : String = "routeroute"
  
  implicit val defaultURI : URI = 
    new URI( "amqp", null, "localhost", 5672, "/mult", "routingKey=routeroute", null )
}
