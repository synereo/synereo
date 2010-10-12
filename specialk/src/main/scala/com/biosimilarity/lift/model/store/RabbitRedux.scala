// -*- mode: Scala;-*- 
// Filename:    RabbitRedux.scala<2> 
// Authors:     lgm                                                    
// Creation:    Mon Oct 11 15:24:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

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
  ) : Int

  def rabbitParams() : ConnectionParameters = {
    val params = new ConnectionParameters
    // All of the params, exchanges, and queues are all just example data.
    params.setUsername("guest")
    params.setPassword("guest")
    params.setVirtualHost("/")
    params.setRequestedHeartbeat(0)
    params
  }

  def rabbitFactory() : ConnectionFactory = {    
    new ConnectionFactory(rabbitParams())
  }

  def rabbitTube(
    cnxnFctry : ConnectionFactory,
    host : String,
    port : Int
  ) = {
    val conn = cnxnFctry.newConnection( host, port )
    val channel = conn.createChannel()
    val ticket = configure( cnxnFctry, host, port )( channel )

    ( conn, channel, ticket )
  }    
}

trait RabbitWriter {
  self : Rabbitter =>

  def send [T] (
    channel : Channel,
    exchange : String,
    routingKey : String,
    ticket : Int
    )(
    msg: T
  ) {
      // Now write an object to a byte array and shove it across the wire.
      val bytes = new ByteArrayOutputStream
      val store = new ObjectOutputStream(bytes)
      store.writeObject(msg)
      store.close
      channel.basicPublish(
	ticket,
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
  ) : Int = {
    val conn = cf.newConnection(host, port)
    val channel = conn.createChannel()
    val ticket = channel.accessRequest("/data")
    ticket
  }

  lazy val _amqp : JSONAMQPDispatcher[String] =
    new JSONAMQPDispatcher[String](
      rabbitFactory(),
      host,
      5672
    )  

  override def amqp() : JSONAMQPDispatcher[String] = {
    _amqp
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

  lazy val _jsonListener = new SimpleJSONListener( )    

  override def jsonListener() : JSONListener = {
    _jsonListener
  }  
}

