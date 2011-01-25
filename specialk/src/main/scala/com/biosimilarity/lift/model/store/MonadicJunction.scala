// -*- mode: Scala;-*- 
// Filename:    MonadicJunction.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 20:36:16 2011 
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
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
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

trait MonadicDTSMsgScope[Namespace,Var,Tag,Value]
extends DTSMsgScope[Namespace,Var,Tag,Value]
{
  import AMQPDefaults._

  object AnAMQPTraceMonitor extends TraceMonitor[Msgs.DReq,Msgs.DRsp]  

  class StdMonadicAgentJSONAMQPDispatcher[TxPort](
    host : String, port : Int,
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
    Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  ) extends StdMonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp](
    host, port
  ) with MonadicAgency[String,Msgs.DReq,Msgs.DRsp] {  
    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp
    override def name : URI = {
      new URI( "agent", host, "/", "" )
    }    
  }

  trait SemiMonadicAgentJSONAMQPTwistedPair[TxPort]
  extends MonadicAgency[TxPort,Msgs.DReq,Msgs.DRsp]
  with SemiMonadicJSONAMQPTwistedPair[Msgs.JTSReqOrRsp] {
    self : MonadicWireToTrgtConversion with MonadicGenerators with WireTap with Journalist =>
      
    override type Trgt = Msgs.JTSReqOrRsp
    override def jsonDispatcher( handle : Msgs.JTSReqOrRsp => Unit )(
      implicit dispatchOnCreate : Boolean, port : Int
    ) : StdMonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp] = {
      _jsonDispatcher match {
	case Some( jd ) => jd
	case None => {
	  val jd =
	    new StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp](
	      srcURI.getHost, port,
	      new ListBuffer[Msgs.JTSReq](),
	      new ListBuffer[Msgs.JTSRsp](),
	      Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
	      AnAMQPTraceMonitor
	    )
	  
	  if ( dispatchOnCreate ) {
	    reset {
	      for( msg <- jd.xformAndDispatch( jd.beginService() ) ){
		handle( msg )
	      }
	    }	
	  }
	  
	  _jsonDispatcher = Some( jd )
	  jd
	}
      }
    }
  }

  class SMAJATwistedPair(
    override val srcURI : URI,
    override val trgtURI : URI
  ) extends SemiMonadicAgentJSONAMQPTwistedPair[String] 
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion with MonadicGenerators with WireTap
  with Journalist
  with UUIDOps {
      override type Wire = String
      override type Trgt = Msgs.JTSReqOrRsp
      override def tap [A] ( fact : A ) : Unit = {
	reportage( fact )
      }
      def jsonDispatcher() : StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp] = {
	jsonDispatcher( ( x ) => {} ).asInstanceOf[StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp]]
      }
      override def name : URI = srcURI
      override def requests : ListBuffer[Msgs.JTSReq] = {
	jsonDispatcher().requests
      }
      override def responses : ListBuffer[Msgs.JTSRsp] = {
	jsonDispatcher( ).responses
      }
      override def nameSpace : Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]] = {
	jsonDispatcher( ).nameSpace
    }
      override def traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp] =
    {
      jsonDispatcher( ).traceMonitor
    }

    def send( dreq : Msgs.DReq ) : Unit = {
      reportage(
	(
	  this
	  + " is sending : "
	  + dreq
	  + " on behalf of "
	  + srcURI
	  + " to "
	  + trgtURI
	)
      )
      val jr = JustifiedRequest[Msgs.DReq,Msgs.DRsp](
	getUUID(),
	trgtURI,
	srcURI,
	getUUID(),
	dreq,
	None
      )
      
      send( Left[Msgs.JTSReq,Msgs.JTSRsp]( jr ) )
    }

    def send( drsp : Msgs.DRsp ) : Unit = {
      reportage(
	(
	  this
	  + " is sending : "
	  + drsp
	  + " on behalf of "
	  + srcURI
	  + " to "
	  + trgtURI
	)
      )
      val jr = JustifiedResponse[Msgs.DReq,Msgs.DRsp](
	getUUID(),
	trgtURI,
	srcURI,
	getUUID(),
	drsp,
	None
      )
      
      send( Right[Msgs.JTSReq,Msgs.JTSRsp]( jr ) )
    }
  }

  object SMAJATwistedPair {
    def apply (
      srcIPStr : String, trgtIPStr : String
    ) : SMAJATwistedPair = {
      new SMAJATwistedPair(
	new URI( "agent", srcIPStr, "/", "" ),
	new URI( "agent", trgtIPStr, "/", "" )
      )
    }
    def unapply(
      smajatp : SMAJATwistedPair
    ) : Option[(URI,URI)] = {
      Some( ( smajatp.srcURI, smajatp.trgtURI ) )
    }    
  }

  trait MonadicCollective 
  extends MonadicAgency[String,Msgs.DReq,Msgs.DRsp] {
    self : MonadicWireToTrgtConversion
	with MonadicGenerators with WireTap with Journalist =>
    
    def agentTwistedPairs :
    Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]]
    def acquaintances : Seq[URI]
    
    def meetNGreet( acquaintances : Seq[URI] )
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      {
	val map = new HashMap[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]]()
	for( acquaintance <- acquaintances )
	yield {
	  val atp =
	    new SMAJATwistedPair(
	      name,
	      acquaintance
	    )      
	  
	  map( acquaintance ) = atp	

	}
	map
      }
  }

  class MonadicJunction(
    override val name : URI,
    override val acquaintances : Seq[URI],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
      Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  )
  extends TermStore[Namespace,Var,Tag,Value](
  ) with MonadicCollective
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators
  with WireTap
  with Journalist {
    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    override lazy val agentTwistedPairs
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      meetNGreet( acquaintances )

    def forwardGet( path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
      for( ( uri, jsndr ) <- agentTwistedPairs ) {
	reportage(
	  (
	    this
	    + " forwarding to "
	    + uri
	  )
	)
	val smajatp : SMAJATwistedPair =
	  jsndr.asInstanceOf[SMAJATwistedPair]

	smajatp.send(
	  Msgs.MDGetRequest[Namespace,Var,Tag,Value]( path ).asInstanceOf[Msgs.DReq]
	)
      }
    }
  }

  class InMemoryMonadicJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends MonadicJunction(
    name,
    acquaintances,
    new ListBuffer[Msgs.JTSReq](),
    new ListBuffer[Msgs.JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
    AnAMQPTraceMonitor
  ) {
    def handleRequest( dreq : Msgs.JTSReq ) : Unit = {
      dreq match {
	case JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  ) => {
	    body match {
	      case dgreq@Msgs.MDGetRequest( path ) => {
		val k =
		  {
		    ( v : Option[Resource] ) => {
		      //tap( v )
		      for(
			atp <- agentTwistedPairs.get( msrc );
			value <- v
		      ) {
			val smajatp : SMAJATwistedPair =
			  atp.asInstanceOf[SMAJATwistedPair]

			value match {
			  case Ground( gv ) =>
			    smajatp.send(
			      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
				path,
				gv
			      )
			    )
			}
		      }
		      v
		    }
		  }
	      }
	      case dfreq@Msgs.MDFetchRequest( path ) => {
		reportage(
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
			val smajatp : SMAJATwistedPair =
			  atp.asInstanceOf[SMAJATwistedPair]

			value match {
			  case Ground( gv ) =>
			    smajatp.send(
			      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
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
	      case dpreq@Msgs.MDPutRequest( path, value ) => {	
		reportage(
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
    }

    def handleResponse( drsp : Msgs.JTSRsp ) : Unit = {
      drsp match {
	case JustifiedResponse( 
	  msgId, mtrgt, msrc, lbl, body, _
	) => {
	  body match {
	    case Msgs.MDGetResponse( path, value ) => {
	      put( path, value )
	    }
	    case Msgs.MDFetchResponse( path, value ) => {
	      put( path, value )
	    }
	    case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	    }
	    case _ => {
	      reportage(
		(
		  this 
		  + " handling unexpected message : "
		  + body
		)
	      )
	    }
	  }
	}
      }
    }

    def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit = {
      dmsg match {
	case Left(
	  dreq@JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  reportage(
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
	  handleRequest( dreq )
	}
	case Right(
	  drsp@JustifiedResponse( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  reportage(
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
	  handleResponse( drsp )
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
		    reportage(
		      (
			this
			+ " storing continuation to wait for value : "
			+ k
		      )
		    )
		    _waiters( place ) =
		      _waiters.get( place )
		    .getOrElse( Nil ) ++ List( k )
		    
		    reportage(
		      (
			this 
			+ " forwarding to acquaintances "
		      )
		    )
		    forwardGet( path )
		    
		    k( None )
		  }	    	      
		}
	      reportage(
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
}

object MonadicMsgJunction
  extends MonadicDTSMsgScope[String,String,String,String]
  with UUIDOps
{
  type MsgTypes = DTSMSH[String,String,String,String]
  val aLabelUUID = getUUID()
  val bLabelUUID = getUUID()

  val aLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	aLabelUUID.toString.replace( "-", "" )
      )
    )
  val bLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	bLabelUUID.toString.replace( "-", "" )
      )
    )

  val protoDreqUUID = getUUID()
  val protoDrspUUID = getUUID()    
  
  object MonadicDMsgs extends MsgTypes {
    
    override def protoDreq : DReq = MDGetRequest( aLabel )
    override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
    override def protoJtsreq : JTSReq =
      JustifiedRequest(
	protoDreqUUID,
	new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	getUUID(),
	protoDreq,
	None
      )
    override def protoJtsrsp : JTSRsp = 
      JustifiedResponse(
	protoDreqUUID,
	new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	getUUID(),
	protoDrsp,
	None
      )
    override def protoJtsreqorrsp : JTSReqOrRsp =
      Left( protoJtsreq )
  }
  
  override def protoMsgs : MsgTypes = MonadicDMsgs
}
