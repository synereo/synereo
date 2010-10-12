// -*- mode: Scala;-*- 
// Filename:    Tribe.scala 
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

  case object _jsonListener
  extends AMQPAgent( selfIdentity ) {
    override def act () {
      nameSpace match {
	case None => {
	  logError( name, this, NoNamespace() )
	}
	case Some( map ) => {
	  receive {
	    case msg@AMQPMessage( cntnt : String ) => {
	      val h2o = rehydrate( cntnt ) 
	      h2o match { 
		case Left( jreq ) => this ! jreq 
		case Right( jrsp ) => this ! jrsp
	      }
	    }
	    case jr@JustifiedRequest(
	      m, p, d, t,
	      f : JSONOverAMQP,
	      c : Option[Response[AbstractJustifiedRequest[JSONOverAMQP,JSONOverAMQP],JSONOverAMQP]]
	    ) => {	    
	      val jrJSON : JustifiedRequest[JSONOverAMQP,JSONOverAMQP]
	      = jr.asInstanceOf[JustifiedRequest[JSONOverAMQP,JSONOverAMQP]]
    
	      if ( validate( jrJSON ) ) {
		println( "calling handle on " + jr )
		reset {
		  shift {
		    ( k : Status[JustifiedRequest[JSONOverAMQP,JSONOverAMQP]] => Status[JustifiedRequest[JSONOverAMQP,JSONOverAMQP]] )
		  => {
		    k( handleWithContinuation( jrJSON, k ) )
		  }
		  }
		}
	      }
	      act()
	    }
	    case jr@JustifiedResponse(
	      m, p, d, t,
	      f : JSONOverAMQP,
	      c : Option[Request[AbstractJustifiedResponse[JSONOverAMQP,JSONOverAMQP],JSONOverAMQP]]
	    ) =>  {
	      val jrJSON : JustifiedResponse[JSONOverAMQP,JSONOverAMQP]
	      = jr.asInstanceOf[JustifiedResponse[JSONOverAMQP,JSONOverAMQP]]
	      if ( validate( jrJSON ) ) {
		println( "calling handle on " + jr )
		reset {
		  shift {
		    ( k : Status[JustifiedResponse[JSONOverAMQP,JSONOverAMQP]] => Status[JustifiedResponse[JSONOverAMQP,JSONOverAMQP]] )
		  => {
		    k( handleWithContinuation( jrJSON, k ) )
		  }
		  }
		}
	      }
	      act()
	    }
	    case ir@InspectRequests( t, f ) => {
	      if ( validate( ir ) ) {
		println( "calling handle on " + ir )
		handle( ir )
	      }
	      act()
	    }
	    case ir@InspectResponses( t, f ) => {
	      if ( validate( ir ) ) {
		println( "calling handle on " + ir )
		handle( ir )
	      }
	      act()
	    }
	    case ir@InspectNamespace( t, f ) => {
	      if ( validate( ir ) ) {
		println( "calling handle on " + ir )
		handle( ir )
	      }
	      act()
	    }
	  }
	}
      }    
    }
  }

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

  def rehydrate( contents: String ) :
  Either[
    JustifiedRequest[JSONOverAMQP,JSONOverAMQP],
    JustifiedResponse[JSONOverAMQP,JSONOverAMQP]
  ] = {
    val msg =
      new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
    msg match {
      case jreq : JustifiedRequest[JSONOverAMQP,JSONOverAMQP] => {
	Left[JustifiedRequest[JSONOverAMQP,JSONOverAMQP],JustifiedResponse[JSONOverAMQP,JSONOverAMQP]]( jreq )
      }
      case jrsp : JustifiedResponse[JSONOverAMQP,JSONOverAMQP] => {
	Right[JustifiedRequest[JSONOverAMQP,JSONOverAMQP],JustifiedResponse[JSONOverAMQP,JSONOverAMQP]]( jrsp )
      }
      case _ => {
	throw new Exception(
	  "unexpected message type : " + msg.getClass
	)
      }
    }
  }
  
}
