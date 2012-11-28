// -*- mode: Scala;-*- 
// Filename:    RabbitRedux.scala<2> 
// Authors:     lgm                                                    
// Creation:    Mon Oct 11 15:24:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.deprecated

import com.biosimilarity.lift.lib._

import amqp.RabbitFactory
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.collection.Map
import scala.collection.mutable.HashMap
import scala.actors.Actor
import scala.actors.Actor._

import com.rabbitmq.client._

import org.prolog4j._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait Rabbitter {
  def configure(
    cf : ConnectionFactory,
    host : String,
    port : Int
  )(
    channel: Channel
  )

  def rabbitFactory() : ConnectionFactory = {
    val factory = new ConnectionFactory()
    factory.setUsername("guest")
    factory.setPassword("guest")
    factory.setVirtualHost("/")
    factory.setRequestedHeartbeat(0)
    factory
  }

  def rabbitTube(
    factory : ConnectionFactory,
    host : String,
    port : Int
  ) = {
    val connection = RabbitFactory.getConnection(factory, host, port)
    val channel = connection.createChannel()
//    configure( factory, host, port )( channel )

    ( connection, channel )
  }    
}

trait RabbitWriter {
  self : Rabbitter =>

  def send [T] (
    channel : Channel,
    exchange : String,
    routingKey : String
    )(
    msg: T
  ) {
      // Now write an object to a byte array and shove it across the wire.
      val bytes = new ByteArrayOutputStream
      val store = new ObjectOutputStream(bytes)
      store.writeObject(msg)
      store.close
      channel.basicPublish(
//	ticket,
	exchange,
	routingKey,
	null,
	bytes.toByteArray
      )
    }
}

trait JSONOverAMQPListener {
  type JSONListener <: Actor
  def host : String
  def amqp() : JSONAMQPDispatcher[String]
  def jsonListener() : JSONListener
  def startAMQPDispatcher() : Unit
  def addJSONListener() : Unit
}

trait AbstractJSONAMQPListener
extends Rabbitter
with JSONOverAMQPListener {    
  def configure(
    cf : ConnectionFactory,
    host : String,
    port : Int
  )(
    channel: Channel
  ) : Unit = {
    //BUGBUG: JSK - is this internal code needed anymore now that ticket is obsolete
    val conn = RabbitFactory.getConnection(cf, host, port)
    val channel = conn.createChannel()
  }

  // lazy val _amqp : JSONAMQPDispatcher[String] =
//     new JSONAMQPDispatcher[String](
//       rabbitFactory(),
//       host,
//       5672
//     )  

  var _amqp : Option[JSONAMQPDispatcher[String]] = None    

  override def amqp() : JSONAMQPDispatcher[String] = {
    _amqp match {
      case Some( dispatcher ) => dispatcher
      case None => {
	val dispatcher =
	  new JSONAMQPDispatcher[String](
	    rabbitFactory(),
	    host,
	    5672
	  )
	_amqp = Some( dispatcher )
	dispatcher
      }
    }
  }

  override def startAMQPDispatcher() : Unit = {
    amqp.start
  }

  override def addJSONListener() : Unit = {
    jsonListener().start
    amqp ! AMQPAddListener( jsonListener() )
  }    
}

trait SimpleJSONAMQPListener
extends Rabbitter
with AbstractJSONAMQPListener {  

  // JSON Listener
  class SimpleJSONListener(
  ) extends Actor
  with JSONToScalaHandler
  with UUIDOps {    
    def act = {
      react {
	case msg@AMQPMessage( contents : String ) => {
	  handle( "dummy" )( contents );
	  act
	}
      }
    }
  }

  type JSONListener = SimpleJSONListener  

  //lazy val _jsonListener = new SimpleJSONListener( )    
  var _jsonListener : Option[JSONListener] = None

  override def jsonListener() : JSONListener = {
    _jsonListener match {
      case Some( jl ) => jl
      case None => {
	val jl = new SimpleJSONListener( )    
	_jsonListener = Some( jl )
	jl
      }
    }
  }  
}

case class HostedSimpleJSONListener( override val host : String ) 
extends SimpleJSONAMQPListener {
  startAMQPDispatcher()
  addJSONListener()
}
