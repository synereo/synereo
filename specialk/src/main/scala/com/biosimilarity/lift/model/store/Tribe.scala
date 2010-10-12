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

trait OZ[GetRequest,GetContinuation] {
  def hostToOutstandingReqs : Map[URI,(GetRequest,GetContinuation)]
}

trait Collective {
  def selfIdentity : URI
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
  override val selfIdentity : URI,
  override val jsonSender : Seq[StdJSONOverAMQPSender],
  override val jsonDispatcher : Seq[JSONAMQPListener],
  override val acquaintances : Seq[URI]
) extends TermStore[Namespace,Var,Tag,Value]
with Collective
with AgentsOverAMQP
with AbstractJSONAMQPListener {
  
  type JSONListener = AMQPAgent  

  lazy val _jsonListener = new AMQPAgent( selfIdentity )    

  override def jsonListener() : JSONListener = {
    _jsonListener
  }  

  override def host : String = { selfIdentity.getHost }

  trait DMsg
  case class DGetRequest( path : CnxnCtxtLabel[Namespace,Var,Tag] )
       extends DMsg
  case class DGetResponse(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends DMsg

  def forwardGet( path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
    for( jsndr <- jsonSender ) {
      jsndr.send( DGetRequest( path ) )
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
