// -*- mode: Scala;-*- 
// Filename:    SpecialKMessenger.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 13:58:10 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import net.liftweb.amqp._

import scala.collection.mutable._
import scala.util.continuations._ 

import scala.actors.Actor
import scala.actors.Actor._

import com.rabbitmq.client._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import java.util.UUID

trait DistributedTermSpaceMsg[Namespace,Var,Tag,Value]
case class DGetRequest[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag]
) extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]
case class DGetResponse[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag],
  value : Value
) extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

trait AgentsOverAMQP[Namespace,Var,Tag,Value] {
  type DMsg = DistributedTermSpaceMsg[Namespace,Var,Tag,Value]
  type JTSReq = JustifiedRequest[DMsg,DMsg]
  type JTSRsp = JustifiedResponse[DMsg,DMsg]

  object IdVendor extends UUIDOps {
    def getURI() = {
      new URI( "com", "biosimilarity", getUUID().toString )
    }
  }

  object AnAMQPTraceMonitor extends TraceMonitor[DMsg,DMsg]  

  class AMQPAgent(
    alias : URI
  ) extends ReflectiveMessenger[DMsg,DMsg](
    alias,
    new ListBuffer[JTSReq](),
    new ListBuffer[JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[DMsg,DMsg]]),
    AnAMQPTraceMonitor
  )
  with UUIDOps {
    override def validateTarget( msg : {def to : URI} ) : Boolean = {
      // Put URI filtering behavior here
      true
    }
    
    override def validateAcquaintance( msg : {def from : URI} ) : Boolean = {
      // Put Requestor filtering behavior here
      nameSpace match {
	case None => false
	case Some( map ) => true
      }
    }

    override def handleWithContinuation(
      request : JTSReq,
      k : Status[JTSReq] => Status[JTSReq]
    ) = {
      //println( "handling: " + request )
      request match {
	case JustifiedRequest(
	  msgId, trgt, src, lbl, body, None
	) => { 
	  // Handle a justified request with no initiating response
	}
	case _ => {
	  // Handle a justified request with an initiating response
	}
      }
      JReqStatus(
	request,
	true,
	None,
	Some( k )
      )
    }  
  }
  
}

class AgentTwistedPair[Namespace,Var,Tag,Value](
  src : URI,
  trgt : URI
) extends AgentsOverAMQP[Namespace,Var,Tag,Value]
with AbstractJSONAMQPListener
with UUIDOps {
  
  type JSONListener = AMQPAgent  

  case object _jsonListener
  extends AMQPAgent( trgt ) {
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
	      f : DMsg,
	      c : Option[Response[AbstractJustifiedRequest[DMsg,DMsg],DMsg]]
	    ) => {	    
	      val jrJSON : JustifiedRequest[DMsg,DMsg]
	      = jr.asInstanceOf[JustifiedRequest[DMsg,DMsg]]
    
	      if ( validate( jrJSON ) ) {
		println( "calling handle on " + jr )
		reset {
		  shift {
		    ( k : Status[JustifiedRequest[DMsg,DMsg]] => Status[JustifiedRequest[DMsg,DMsg]] )
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
	      f : DMsg,
	      c : Option[Request[AbstractJustifiedResponse[DMsg,DMsg],DMsg]]
	    ) =>  {
	      val jrJSON : JustifiedResponse[DMsg,DMsg]
	      = jr.asInstanceOf[JustifiedResponse[DMsg,DMsg]]
	      if ( validate( jrJSON ) ) {
		println( "calling handle on " + jr )
		reset {
		  shift {
		    ( k : Status[JustifiedResponse[DMsg,DMsg]] => Status[JustifiedResponse[DMsg,DMsg]] )
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

  override def host : String = { trgt.getHost }

  case object _jsonSender
  extends JSONAMQPSender(
    rabbitFactory(),
    host,
    5672,
    "mult",
    "routeroute"
  )
  _jsonSender.start
  
  def jsonSender() : JSONAMQPSender = {
    _jsonSender
  }

  def send( contents : DMsg ) : Unit = {
    val jr = JustifiedRequest[DMsg,DMsg](
      getUUID(),
      trgt,
      src,
      getUUID(),
      contents,
      None
    )

    _jsonSender ! AMQPMessage(
      new XStream( new JettisonMappedXmlDriver() ).toXML( jr )
    )
  }  

  def rehydrate( contents: String ) :
  Either[
    JustifiedRequest[DMsg,DMsg],
    JustifiedResponse[DMsg,DMsg]
  ] = {
    val msg =
      new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
    msg match {
      case jreq : JustifiedRequest[DMsg,DMsg] => {
	Left[JustifiedRequest[DMsg,DMsg],JustifiedResponse[DMsg,DMsg]]( jreq )
      }
      case jrsp : JustifiedResponse[DMsg,DMsg] => {
	Right[JustifiedRequest[DMsg,DMsg],JustifiedResponse[DMsg,DMsg]]( jrsp )
      }
      case _ => {
	throw new Exception(
	  "unexpected message type : " + msg.getClass
	)
      }
    }
  }
  
}


