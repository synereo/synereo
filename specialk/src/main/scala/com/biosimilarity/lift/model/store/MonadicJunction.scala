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

  trait SemiMonadicAgentJSONAMQPTwistedPair[TxPort,Namespace,Var,Tag,Value]
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

  class SMAJATwistedPair[Namespace,Var,Tag,Value](
    override val srcURI : URI,
    override val trgtURI : URI
  ) extends
    SemiMonadicAgentJSONAMQPTwistedPair[String,Namespace,Var,Tag,Value] 
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
    def apply[Namespace,Var,Tag,Value] (
      srcIPStr : String, trgtIPStr : String
    ) : SMAJATwistedPair[Namespace,Var,Tag,Value] = {
      new SMAJATwistedPair[Namespace,Var,Tag,Value](
	new URI( "agent", srcIPStr, "/", "" ),
	new URI( "agent", trgtIPStr, "/", "" )
      )
    }
    def unapply[Namespace,Var,Tag,Value](
      smajatp : SMAJATwistedPair[Namespace,Var,Tag,Value]
    ) : Option[(URI,URI)] = {
      Some( ( smajatp.srcURI, smajatp.trgtURI ) )
    }    
  }

  trait MonadicCollective[Namespace,Var,Tag,Value] 
  extends MonadicAgency[String,Msgs.DReq,Msgs.DRsp] {
    self : MonadicWireToTrgtConversion
	with MonadicGenerators with WireTap with Journalist =>
    
    def agentTwistedPairs :
    Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String,Namespace,Var,Tag,Value]]
    def acquaintances : Seq[URI]
    
    def meetNGreet( acquaintances : Seq[URI] )
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String,Namespace,Var,Tag,Value]] =
      {
	val map = new HashMap[URI,SemiMonadicAgentJSONAMQPTwistedPair[String,Namespace,Var,Tag,Value]]()
	for( acquaintance <- acquaintances )
	yield {
	  val atp =
	    new SMAJATwistedPair[Namespace,Var,Tag,Value](
	      name,
	      acquaintance
	    )      
	  
	  map( acquaintance ) = atp	

	}
	map
      }
  }

  class MonadicJunction[Namespace,Var,Tag,Value](
    override val name : URI,
    override val acquaintances : Seq[URI],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
      Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  ) extends TermStore[Namespace,Var,Tag,Value]
  with MonadicCollective[Namespace,Var,Tag,Value]
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators
  with WireTap
  with Journalist {
    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    override lazy val agentTwistedPairs
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String,Namespace,Var,Tag,Value]] =
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
	val smajatp : SMAJATwistedPair[Namespace,Var,Tag,Value] =
	  jsndr.asInstanceOf[SMAJATwistedPair[Namespace,Var,Tag,Value]]

	smajatp.send(
	  Msgs.DGetRequest[Namespace,Var,Tag,Value]( path ).asInstanceOf[Msgs.DReq]
	)
      }
    }
  }
  
}
