// -*- mode: Scala;-*- 
// Filename:    Junction.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct  8 17:29:51 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.deprecated

import com.biosimilarity.lift.model.store._

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
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
  //def hostToOutstandingReqs : Map[URI,(GetRequest,GetContinuation)]
  def hostToOutstandingReqs : Map[Moniker,(GetRequest,GetContinuation)]
}

trait Collective[Namespace,Var,Tag,Value]
extends EndPoint[Namespace,Var,Tag,Value] {
  //def selfIdentity : URI
  def selfIdentity : Moniker
  //def agentTwistedPairs : Map[URI,AgentTwistedPair[Namespace,Var,Tag,Value]]
  def agentTwistedPairs : Map[Moniker,AgentTwistedPair[Namespace,Var,Tag,Value]]
  //def acquaintances : Seq[URI]
  def acquaintances : Seq[Moniker]

  //override def location : URI = selfIdentity
  override def location : Moniker = selfIdentity

  //case class URIEndPointWrapper( override val location : URI )
  case class URIEndPointWrapper( override val location : Moniker )
  extends EndPoint[Namespace,Var,Tag,Value] {
    def handleRequest( 
      dmsg : JustifiedRequest[DistributedTermSpaceRequest[Namespace,Var,Tag,Value],DistributedTermSpaceResponse[Namespace,Var,Tag,Value]]
    ) : Boolean = {
      throw new Exception( "Attempt to have a reference handle a request" )
    }
    def handleResponse( 
      dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
    ) : Boolean = {
      throw new Exception( "Attempt to have a reference handle a response" )
    }
  }

  implicit def URIasEndPoint(
    //uri : URI
    uri : Moniker
  ) : EndPoint[Namespace,Var,Tag,Value] = {
    URIEndPointWrapper( uri )
  }    
  
  //def meetNGreet( acquaintances : Seq[URI] )
  def meetNGreet( acquaintances : Seq[Moniker] )
  : //Map[URI,AgentTwistedPair[Namespace,Var,Tag,Value]] =
  Map[Moniker,AgentTwistedPair[Namespace,Var,Tag,Value]] =
  {
    //val map = new
    //HashMap[URI,AgentTwistedPair[Namespace,Var,Tag,Value]]()
    val map = new HashMap[Moniker,AgentTwistedPair[Namespace,Var,Tag,Value]]()
    for( acquaintance <- acquaintances )
    yield {
      val atp =
	new AgentTwistedPair[Namespace,Var,Tag,Value](
	  this,
	  acquaintance
	)      

      map( acquaintance ) = atp	

      //atp.startAMQPDispatcher()
      //atp.addJSONListener()
    }
    map
  }
}

class Junction[Namespace,Var,Tag,Value](
  //override val selfIdentity : URI,  
  override val selfIdentity : Moniker,  
  //override val acquaintances : Seq[URI]
  override val acquaintances : Seq[Moniker]
) extends TermStore[Namespace,Var,Tag,Value]
with Collective[Namespace,Var,Tag,Value] {  
  override lazy val agentTwistedPairs
  : //Map[URI,AgentTwistedPair[Namespace,Var,Tag,Value]] =
  Map[Moniker,AgentTwistedPair[Namespace,Var,Tag,Value]] =
    meetNGreet( acquaintances )
  
  def forwardGet( path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
    for( ( uri, jsndr ) <- agentTwistedPairs ) {
      BasicLogService.reportage(
	(
	  this
	  + " forwarding to "
	  + uri
	)
      )
      jsndr.send( DGetRequest[Namespace,Var,Tag,Value]( path ) )
    }
  }

  override def handleRequest(
    dmsg : JustifiedRequest[DistributedTermSpaceRequest[Namespace,Var,Tag,Value],DistributedTermSpaceResponse[Namespace,Var,Tag,Value]]
  ) : Boolean = {
    dmsg match {
      case JustifiedRequest(
	msgId, mtrgt, msrc, lbl, body, _
      ) => { 
	BasicLogService.reportage(
	  (
	    this
	    + " handling : "
	    + dmsg
	    + " from "
	    + msrc
	    + " on behalf of "
	    + mtrgt
	  )
	)
	// Handle a justified request with no initiating response	  
	body match {
	  case dgreq@DGetRequest( path ) => {
	    BasicLogService.reportage(
	      ( 
		this 
		+ " handling : "
		+ dgreq
	      )
	    )
	    val k =
	      {
		( v : Option[Resource] ) => {
		  //tap( v )
		  for(
		    atp <- agentTwistedPairs.get( msrc );
		    value <- v
		  ) {
		    value match {
		      case Ground( gv ) =>
			atp.send(
			  DGetResponse[Namespace,Var,Tag,Value](
			    path,
			    gv
			  )
			)
		    }
		  }
		  v
		}
	      }
	    get( path, k )
	  }
	  case dfreq@DFetchRequest( path ) => {
	    BasicLogService.reportage(
	      (
		this 
		+ "handling : "
		+ dfreq
	      )
	    )
	    val k =
	      {
		( v : Option[Resource] ) => {
		  //tap( v )
		  for(
		    atp <- agentTwistedPairs.get( msrc );
		    value <- v
		  ) {
		    value match {
		      case Ground( gv ) =>
			atp.send(
			  DGetResponse[Namespace,Var,Tag,Value](
			    path,
			    gv
			  )
			)
		    }
		  }
		  v
		}
	      }
	    fetch( path, k )
	  }
	  case dpreq@DPutRequest( path, value ) => {	
	    BasicLogService.reportage(
	      (
		this
		+ " handling : "
		+ dpreq
	      )
	    )
	    put( path, value )
	  }
	}
      }
    }    
    true
  }

  override def handleResponse(
    dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  ) : Boolean = {
    dmsg match {
      case DGetResponse( path, value ) => {
	put( path, value )
      }
      case DFetchResponse( path, value ) => {
	put( path, value )
      }
      case dput : DPutResponse[Namespace,Var,Tag,Value] => {	
      }
      case _ => {
	BasicLogService.reportage(
	  (
	    this 
	    + " handling unexpected message : "
	    + dmsg
	  )
	)
      }
    }
    true
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
		  BasicLogService.reportage(
		    (
		      this
		      + " storing continuation to wait for value : "
		      + k
		    )
		  )
		  _waiters( place ) =
		    _waiters.get( place )
			    .getOrElse( Nil ) ++ List( k )
		  
		  BasicLogService.reportage(
		    (
		      this 
		      + " forwarding to acquaintances "
		    )
		  )
		  forwardGet( path )

		  k( None )
		}	    	      
	      }
	    BasicLogService.reportage(
	      (
		this
		+ " resuming with value : "
		+ rslt
	      )
	    )
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

object PetticoatJunction {
  import identityConversions._
  val agentOneURI = new URI( "agent", "10.0.1.7", "/invitation", "" )
  val agentTwoURI = new URI( "agent", "10.0.1.5", "/invitation", "" )

  val aLabel = new CnxnLeaf[String,String]( "a" )
  val bLabel = new CnxnLeaf[String,String]( "b" )

  def junctionOne() =
    new Junction[String,String,String,String](
      agentOneURI, List( agentTwoURI )
    )
  def junctionTwo() =
    new Junction[String,String,String,String](
      agentTwoURI, List( agentOneURI )
    )
  def goGetEmTiger( jmkr : Unit => Junction[String,String,String,String] ) = {
    val theJunction = jmkr()
    theJunction.get( aLabel )
  }
}
