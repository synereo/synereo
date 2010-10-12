// -*- mode: Scala;-*- 
// Filename:    Junction.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct  8 17:29:51 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
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

trait Collective[Namespace,Var,Tag,Value]
extends EndPoint[Namespace,Var,Tag,Value] {
  def selfIdentity : URI
  def agentTwistedPairs : Seq[AgentTwistedPair[Namespace,Var,Tag,Value]]
  def acquaintances : Seq[URI]

  override def location : URI = selfIdentity

  case class URIEndPointWrapper( override val location : URI )
  extends EndPoint[Namespace,Var,Tag,Value] {
    def handleResponse( 
      dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
    ) : Boolean = {
      throw new Exception( "Attempt to have a reference handle a response" )
    }
  }

  implicit def URIasEndPoint(
    uri : URI
  ) : EndPoint[Namespace,Var,Tag,Value] = {
    URIEndPointWrapper( uri )
  }  

  def tunnel() : Seq[AgentTwistedPair[Namespace,Var,Tag,Value]] = {
    for( acquaintance <- acquaintances )
    yield {
      (
	new AgentTwistedPair[Namespace,Var,Tag,Value](
	  this,
	  acquaintance
	)
      )
    }
  }
}

class Junction[Namespace,Var,Tag,Value](
  override val selfIdentity : URI,
  override val agentTwistedPairs
  : Seq[AgentTwistedPair[Namespace,Var,Tag,Value]],
  override val acquaintances : Seq[URI]
) extends TermStore[Namespace,Var,Tag,Value]
with Collective[Namespace,Var,Tag,Value] {  

  def forwardGet( path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
    for( jsndr <- agentTwistedPairs ) {
      jsndr.send( DFetchRequest[Namespace,Var,Tag,Value]( path ) )
    }
  }

  override def handleResponse(
    dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  ) : Boolean = {
    dmsg match {
      case dget : DGetResponse[Namespace,Var,Tag,Value] => {
	true
      }
      case dfetch : DFetchResponse[Namespace,Var,Tag,Value] => {
	true
      }
      case dput : DPutResponse[Namespace,Var,Tag,Value] => {
	true
      }
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
