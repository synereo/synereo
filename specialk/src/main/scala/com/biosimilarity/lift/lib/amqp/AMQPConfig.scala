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

trait AMQPMonikerOps {
  def mnkrHost( src : Moniker ) : String = src.getHost
  def mnkrPort( src : Moniker ) : Int = src.getPort
  def mnkrExchange( src : Moniker ) : String = {
    val spath = src.getPath.split( "/" )
    spath.length match {
      case 0 => AMQPDefaults.defaultExchange
      case 1 => AMQPDefaults.defaultExchange
      case 2 => spath( 1 )
      case n : Int if ( n > 2 ) => spath( 1 )
    }
  }
  def mnkrRoutingKey( src : Moniker ) : String = {
    src.getQuery match {
      case null => AMQPDefaults.defaultRoutingKey
      case qs => {
	val rkA = qs.split( "," ).filter( ( p : String ) => p.contains( "routingKey" ) )
	rkA.length match {
	  case 0 => AMQPDefaults.defaultRoutingKey
	  case _ => rkA( 0 ).split( "=" )( 1 )
	}
      }
    }    
  }  
}

trait AMQPURIOps {
  def uriHost( src : URI ) : String = src.getHost
  def uriPort( src : URI ) : Int = src.getPort
  def uriExchange( src : URI ) : String = {
    val spath = src.getPath.split( "/" )
    spath.length match {
      case 0 => AMQPDefaults.defaultExchange
      case 1 => AMQPDefaults.defaultExchange
      case 2 => spath( 1 )
      case n : Int if ( n > 2 ) => spath( 1 )
    }
  }
  def uriRoutingKey( src : URI ) : String = {
    val rkA = src.getQuery.split( "," ).filter( ( p : String ) => p.contains( "routingKey" ) )
    rkA.length match {
      case 0 => AMQPDefaults.defaultRoutingKey
      case _ => rkA( 0 ).split( "=" )( 1 )
    }
  }  
}
