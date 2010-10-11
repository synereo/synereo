// -*- mode: Scala;-*- 
// Filename:    Tribe.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct  8 17:29:51 2010 
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

trait Rabitter {
  def configure(
    cf : ConnectionFactory,
    host : String,
    port : Int
  )(
    channel: Channel
  ) : Int

  def rabbitFactory() : (ConnectionParameters, ConnectionFactory) = {
    val params = new ConnectionParameters
    // All of the params, exchanges, and queues are all just example data.
    params.setUsername("guest")
    params.setPassword("guest")
    params.setVirtualHost("/")
    params.setRequestedHeartbeat(0)
    (params, new ConnectionFactory(params))
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

trait OZ[GetRequest,GetContinuation] {
  def hostToOutstandingReqs : Map[URI,(GetRequest,GetContinuation)]
}

trait Warren {
  def selfIdentity : UUID
  def jsonSender : Seq[StdJSONOverAMQPSender]
  def jsonDispatcher : Seq[JSONAMQPListener]
  def acquaintances : Seq[URI]

  def tunnel() : ( Seq[StdJSONOverAMQPSender], Seq[JSONAMQPListener] ) = {
    val sndrRcvrPairs =
      for( acquaintance <- acquaintances )
      yield {
	(
	  new StdJSONOverAMQPSender( acquaintance.getHost ), 
	  new JSONAMQPListener( acquaintance.getHost )
	)
      }
    sndrRcvrPairs.unzip
  }
}

class Tribe[Namespace,Var,Tag,Value](
  override val selfIdentity : UUID,
  override val jsonSender : Seq[StdJSONOverAMQPSender],
  override val jsonDispatcher : Seq[JSONAMQPListener],
  override val acquaintances : Seq[URI]
) extends TermStore[Namespace,Var,Tag,Value]
with Warren
with DrumTalk {
  
  trait DMsg
  case class DGet( path : CnxnCtxtLabel[Namespace,Var,Tag] )
       extends DMsg

  def forwardGet( path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
    for( jsndr <- jsonSender ) {
      jsndr.send( DGet( path ) )
    }
  }

  override def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  )
  : Seq[Option[Resource]] = {        
    for( placeNSoln <- places( path, Input ) )
    yield {
      val ( place, soln ) = placeNSoln
      _labelMap.get( place ) match {
	case sv @ Some( value ) => {
	  _labelMap -= place
	  sv
	}
	case None => {
	  reset {
	    val rslt : Option[Resource] = 
	      shift {
		( k : GetContinuation ) => {	      
		  _waiters( place ) =
		    _waiters.get( place )
			    .getOrElse( Nil ) ++ List( k )

		  forwardGet( path )

		  k( None )
		}	    	      
	      }
	    reportage( "resuming with value : " + rslt )
	    rslt match {
	      case Some( _ ) =>	next( rslt )
	      case nv @ _ => nv
	    }
	  }
	}	
      }
    }    
  }
}
