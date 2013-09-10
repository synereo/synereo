// -*- mode: Scala;-*- 
// Filename:    SpecialKMessenger.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 13:58:10 2010 
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

import scala.collection.mutable._
import scala.util.continuations._ 

import com.rabbitmq.client._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import java.util.UUID

import identityConversions._

trait AgentsOverAMQP[Namespace,Var,Tag,Value] {
  type DReq = DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  type DRsp = DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  type JTSReq = JustifiedRequest[DReq,DRsp]
  type JTSRsp = JustifiedResponse[DReq,DRsp]

  object IdVendor extends UUIDOps {
    def getURI() = {
      new URI( "com", "biosimilarity", getUUID().toString )
    }
  }

  object AnAMQPTraceMonitor extends TraceMonitor[DReq,DRsp]  

  class AMQPAgent(
    //alias : URI
    alias : Moniker
  ) extends ReflectiveMessenger[DReq,DRsp](
    alias,
    new ListBuffer[JTSReq](),
    new ListBuffer[JTSRsp](),
    //Some( new LinkedHashMap[URI,Socialite[DReq,DRsp]]),
    Some( new LinkedHashMap[Moniker,Socialite[DReq,DRsp]]),
    AnAMQPTraceMonitor
  )
  with UUIDOps {
    //override def validateTarget( msg : {def to : URI} ) : Boolean = {
    override def validateTarget( msg : {def to : Moniker} ) : Boolean = {
      // Put URI filtering behavior here
      true
    }
    
    //override def validateAcquaintance( msg : {def from : URI} ) : Boolean = {
    override def validateAcquaintance( msg : {def from : Moniker} ) : Boolean = {
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

trait EndPoint[Namespace,Var,Tag,Value] {
  //def location : URI
  def location : Moniker
  def handleRequest( 
    dmsg : JustifiedRequest[DistributedTermSpaceRequest[Namespace,Var,Tag,Value],DistributedTermSpaceResponse[Namespace,Var,Tag,Value]]
  ) : Boolean
  def handleResponse( 
    dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  ) : Boolean
}

class EndPointLocuter[Namespace,Var,Tag,Value](
  //override val location : URI
  override val location : Moniker
) extends EndPoint[Namespace,Var,Tag,Value] {
  // val reportage = report( Twitterer() ) _
  override def handleRequest( 
    dmsg : JustifiedRequest[DistributedTermSpaceRequest[Namespace,Var,Tag,Value],DistributedTermSpaceResponse[Namespace,Var,Tag,Value]]
  ) : Boolean = {
    BasicLogService.reportage( this + "is handling : " + dmsg )
    true
  }
  override def handleResponse( 
    dmsg : DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  ) : Boolean = {
    BasicLogService.reportage( this + "is handling : " + dmsg )
    true
  }
}

class AgentTwistedPair[Namespace,Var,Tag,Value](
  val src : EndPoint[Namespace,Var,Tag,Value],
  val trgt : EndPoint[Namespace,Var,Tag,Value]
) extends JSONAMQPTwisted( src.location.getHost, trgt.location.getHost )
with AgentsOverAMQP[Namespace,Var,Tag,Value]
with Rabbitter
with UUIDOps {
  // val reportage = report( Twitterer() ) _

  implicit def endPointAsURI(
    ep : EndPoint[Namespace,Var,Tag,Value]
  ) : URI = {
    ep.location
  }
  implicit def endPointAsMoniker(
    ep : EndPoint[Namespace,Var,Tag,Value]
  ) : Moniker = {
    MURI( ep.location )
  }

  // implicit def hostNameStringAsURI(
//     hostNameStr : String
//   ) : URI = {
//     if ( hostNameStr.split( "\\." ).length > 1 ) {
//       new URI( "agent", hostNameStr, "/", "" )
//     }
//     else {
//       throw new Exception( "not a host name : " + hostNameStr )
//     }
//   }

  type JSONListener = AMQPAgent  

  override def configure(
    cf : ConnectionFactory,
    host : String,
    port : Int
  )(
    channel: Channel
  ) = {
  }

  case class AgentAMQPJSONListener( src : EndPoint[Namespace,Var,Tag,Value] )
  extends AMQPAgent( src ) {
    override def handleWithContinuation(
      request : JTSReq,
      k : Status[JTSReq] => Status[JTSReq]
    ) = {
      BasicLogService.reportage( this + "is handling : " + request )
      request match {
	case JustifiedRequest(
	  msgId, mtrgt, msrc, lbl, body, None
	) => { 
	  // Handle a justified request with no initiating response	  
	  JReqStatus(
	    request,
	    src.handleRequest( request ),
	    None,
	    Some( k )
	  )
	}
	case _ => {
	  // Handle a justified request with an initiating response
	  JReqStatus(
	    request,
	    src.handleRequest( request ),
	    None,
	    Some( k )
	  )
	}
      }      
    }

    override def handleWithContinuation(
      response : JTSRsp,
      k : Status[JTSRsp] => Status[JTSRsp]
    ) = {
      BasicLogService.reportage( this + " is handling : " + response )
      response match {
	case JustifiedResponse(
	  msgId, mtrgt, msrc, lbl, body, just
	) => { 	  
	  JRspStatus(
	    response,
	    src.handleResponse( body ),
	    None,
	    Some( k )
	  )
	}
	case _ => {
	  throw new Exception(
	    "Unexpected message type : " + response.getClass
	  )
	}
      }      
    }

    override def act () {
      nameSpace match {
	case None => {
	  logError( name, this, NoNamespace() )
	}
	case Some( map ) => {
	  receive {
	    case msg@AMQPMessage( cntnt : String ) => {
	      BasicLogService.reportage(
		this + " is handling : " + msg + " with contents :" + cntnt
	      )
	      val h2o = rehydrate( cntnt ) 
	      h2o match { 
		case Left( jreq ) => this ! jreq 
		case Right( jrsp ) => this ! jrsp
	      }
	    }
	    case jr@JustifiedRequest(
	      m, p, d, t,
	      f : DReq,
	      c : Option[Response[AbstractJustifiedRequest[DReq,DRsp],DRsp]]
	    ) => {	    
	      val jrJSON : JustifiedRequest[DReq,DRsp]
	      = jr.asInstanceOf[JustifiedRequest[DReq,DRsp]]
    
	      if ( validate( jrJSON ) ) {
		BasicLogService.reportage(
		  (
		    this
		    + " is calling handleWithContinuation on "
		    + jr
		  )
		)
		reset {
		  shift {
		    ( k : Status[JustifiedRequest[DReq,DRsp]] => Status[JustifiedRequest[DReq,DRsp]] )
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
	      f : DRsp,
	      c : Option[Request[AbstractJustifiedResponse[DReq,DRsp],DReq]]
	    ) =>  {
	      val jrJSON : JustifiedResponse[DReq,DRsp]
	      = jr.asInstanceOf[JustifiedResponse[DReq,DRsp]]
	      if ( validate( jrJSON ) ) {
		BasicLogService.reportage(
		  (
		    this
		    + " is calling handleWithContinuation on "
		    + jr
		  )
		)
		reset {
		  shift {
		    ( k : Status[JustifiedResponse[DReq,DRsp]] => Status[JustifiedResponse[DReq,DRsp]] )
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
		BasicLogService.reportage(
		  (
		    this 
		    + "is calling handle on "
		    + ir
		  )
		)
		handle( ir )
	      }
	      act()
	    }
	    case ir@InspectResponses( t, f ) => {
	      if ( validate( ir ) ) {
		BasicLogService.reportage(
		  (
		    this 
		    + " is calling handle on "
		    + ir
		  )
		)
		handle( ir )
	      }
	      act()
	    }
	    case ir@InspectNamespace( t, f ) => {
	      if ( validate( ir ) ) {
		BasicLogService.reportage(
		  (
		    this 
		    + " is calling handle on "
		    + ir
		  )
		)
		handle( ir )
	      }
	      act()
	    }
	  }
	}
      }    
    }
  }

  var _jsonListener : Option[JSONListener] = None

  def jsonListener() : JSONListener = {
    _jsonListener match {
      case Some( jl ) => jl
      case None => {
	val jl = AgentAMQPJSONListener( src )
	_jsonListener = Some( jl )
	jl.start
	jl
      }
    }
  }  

  def host : String = { src.location.getHost }

  var _jsonSender : Option[JSONAMQPSender] = None 
  
  def jsonSender() : JSONAMQPSender = {
    _jsonSender match {
      case Some( js ) => js
      case None => {
	val js = new JSONAMQPSender(
	  rabbitFactory(),
	  trgt.location.getHost,
	  5672,
	  "mult",
	  "routeroute"
	)       

	_jsonSender = Some( js )

	js.start
	js
      }
    }
  }

  def send( contents : DReq ) : Unit = {    
    // BasicLogService.reportage(
//       (
// 	this
// 	+ " is sending : "
// 	+ contents
// 	+ " on behalf of "
// 	+ src
// 	+ " to "
// 	+ trgt
//       )
//     )
    val jr = JustifiedRequest[DReq,DRsp](
      getUUID(),
      trgt,
      src,
      getUUID(),
      contents,
      None
    )

    jsonSender() ! AMQPMessage(
      new XStream( new JettisonMappedXmlDriver() ).toXML( jr )
    )
  }

  def send( contents : DRsp ) : Unit = {
    // BasicLogService.reportage(
//       (
// 	this
// 	+ " is sending : "
// 	+ contents
// 	+ " on behalf of "
// 	+ src
// 	+ " to "
// 	+ trgt
//       )
//     )

    val jr = JustifiedResponse[DReq,DRsp](
      getUUID(),
      src,
      trgt,
      getUUID(),
      contents,
      None
    )

    jsonSender() ! AMQPMessage(
      new XStream( new JettisonMappedXmlDriver() ).toXML( jr )
    )
  }

  def rehydrate( contents: String ) :
  Either[
    JustifiedRequest[DReq,DRsp],
    JustifiedResponse[DReq,DRsp]
  ] = {
    val msg =
      new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
    msg match {
      case jreq : JustifiedRequest[DReq,DRsp] => {
	Left[JustifiedRequest[DReq,DRsp],JustifiedResponse[DReq,DRsp]]( jreq )
      }
      case jrsp : JustifiedResponse[DReq,DRsp] => {
	Right[JustifiedRequest[DReq,DRsp],JustifiedResponse[DReq,DRsp]]( jrsp )
      }
      case _ => {
	throw new Exception(
	  "unexpected message type : " + msg.getClass
	)
      }
    }
  }  
  
}

object ATPTest {
  implicit def hostNameStringAsEndPoint(
    hostNameStr : String
  ) : EndPoint[String,String,String,String] = {
    if ( hostNameStr.split( "\\." ).length > 1 ) {
      new EndPointLocuter(
	new URI( "agent", hostNameStr, "/", "" )
      )
    }
    else {
      throw new Exception( "not a host name : " + hostNameStr )
    }
  }

  val aLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left[String,String]( "a" )
    )
  val bLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left[String,String]( "b" )
    )
  
  def atp( src : String, trgt : String ) = {
    new AgentTwistedPair[String,String,String,String]( src, trgt )
  }

  def doGetReq( atp : AgentTwistedPair[String,String,String,String] )
  : Unit = {
    atp.send( DGetRequest[String,String,String,String]( aLabel ) )
  }
}
