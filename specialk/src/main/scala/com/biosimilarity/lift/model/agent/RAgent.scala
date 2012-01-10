// -*- mode: Scala;-*- 
// Filename:    RAgent.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 10 16:34:49 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.agent

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.model.msg._

import java.net.URI
import java.util.UUID

import scala.util.continuations._ 
//import scala.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

// Tracking information

trait Status[Msg] {
  def message : Msg
  def success : Boolean
  def explanation : Option[String]
  def continuation : Option[Status[Msg] => Status[Msg]]
}

abstract class HandlingStatus[Msg] (
  message : Msg,
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[Msg] => Status[Msg]]
) extends Status[Msg]

case class JReqStatus[ReqBody,RspBody](
  message : JustifiedRequest[ReqBody,RspBody],
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[JustifiedRequest[ReqBody,RspBody]] => Status[JustifiedRequest[ReqBody,RspBody]]]
) extends HandlingStatus[JustifiedRequest[ReqBody,RspBody]](
  message, success, explanation, continuation
)

case class JRspStatus[ReqBody,RspBody](
  message : JustifiedResponse[ReqBody,RspBody],
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[JustifiedResponse[ReqBody,RspBody]] => Status[JustifiedResponse[ReqBody,RspBody]]]
) extends HandlingStatus[JustifiedResponse[ReqBody,RspBody]](
  message, success, explanation, continuation
)

// Ability to handle requests and responses with corresponding continuations

trait Awareness[ReqBody,RspBody] {
  def handleWithContinuation(
    request : JustifiedRequest[ReqBody,RspBody],
    k : Status[JustifiedRequest[ReqBody,RspBody]] => Status[JustifiedRequest[ReqBody,RspBody]]
  ) : Status[JustifiedRequest[ReqBody,RspBody]] = {
    JReqStatus( request, false, None, Some( k ) )
  }
  def handleWithContinuation(
    response : JustifiedResponse[ReqBody,RspBody],
    k : Status[JustifiedResponse[ReqBody,RspBody]] => Status[JustifiedResponse[ReqBody,RspBody]]
  ) : Status[JustifiedResponse[ReqBody,RspBody]] = {
    JRspStatus( response, false, None, Some( k ) )
  }  
}

trait Focus[ReqBody,RspBody] {
  def handleRequestPayload ( payload : ReqBody ) : Boolean
  def handleResponsePayload ( payload : RspBody ) : Boolean
}

// Mercury -- down to the shiny bits

@transient
class ReflectiveMessenger[ReqBody,RspBody](
  //override val name : URI,
  override val name : Moniker,
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  //override val nameSpace : Option[LinkedHashMap[URI,Socialite[ReqBody,RspBody]]],
  override val nameSpace : Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]],
  override val traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends Actor
     with Socialite[ReqBody,RspBody]
     with Awareness[ReqBody,RspBody]
     with Focus[ReqBody,RspBody] {
  override def useBraceNotation : Boolean = false
  //def likes( dsg : URI, acq : Socialite[ReqBody,RspBody] )
  def likes( dsg : Moniker, acq : Socialite[ReqBody,RspBody] )
  : Boolean = true
       
  override def handleRequestPayload ( payload : ReqBody ) : Boolean = false
  override def handleResponsePayload ( payload : RspBody ) : Boolean = false

  override def handleWithContinuation(
    request : JustifiedRequest[ReqBody,RspBody],
    k : Status[JustifiedRequest[ReqBody,RspBody]] => Status[JustifiedRequest[ReqBody,RspBody]]
  ) : Status[JustifiedRequest[ReqBody,RspBody]] = {
    JReqStatus( request, handleRequestPayload( request.body ), None, Some( k ) )
  }
  override def handleWithContinuation(
    response : JustifiedResponse[ReqBody,RspBody],
    k : Status[JustifiedResponse[ReqBody,RspBody]] => Status[JustifiedResponse[ReqBody,RspBody]]
  ) : Status[JustifiedResponse[ReqBody,RspBody]] = {
    JRspStatus( response, handleResponsePayload( response.body ), None, Some( k ) )
  }  

  def act () {
    nameSpace match {
      case None => {
	logError( name, this, NoNamespace() )
      }
      case Some( map ) => {
	receive {
	  case JustifiedRequest(
	    m, p, d, t,
	    f : ReqBody,
	    c : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
	  ) => {
	    val jr = JustifiedRequest[ReqBody,RspBody]( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      reset {
		shift {
		  ( k : Status[JustifiedRequest[ReqBody,RspBody]] => Status[JustifiedRequest[ReqBody,RspBody]] )
		  => {
		    k( handleWithContinuation( jr, k ) )
		  }
		}
	      }
	    }
	    act()
	  }
	  case JustifiedResponse(
	    m, p, d, t,
	    f : RspBody,
	    c : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
	  ) =>  {
	    val jr = JustifiedResponse[ReqBody,RspBody]( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      reset {
		shift {
		  ( k : Status[JustifiedResponse[ReqBody,RspBody]] => Status[JustifiedResponse[ReqBody,RspBody]] )
		  => {
		    k( handleWithContinuation( jr, k ) )
		  }
		}
	      }
	    }
	    act()
	  }
	  case InspectRequests( t, f ) => {
	    val ir = InspectRequests( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectResponses( t, f ) => {
	    val ir = InspectResponses( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectNamespace( t, f ) => {
	    val ir = InspectNamespace( t, f )
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

object ReflectiveMessengerExtractor {
  def unapply[ReqBody,RspBody]( rmsngr : ReflectiveMessenger[ReqBody,RspBody] )
  : (
      //URI,
      Moniker,
      ListBuffer[JustifiedRequest[ReqBody,RspBody]],
      ListBuffer[JustifiedResponse[ReqBody,RspBody]],
      //Option[LinkedHashMap[URI,Socialite[ReqBody,RspBody]]],
      Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]],
      TraceMonitor[ReqBody,RspBody]
    ) = {
      (
	rmsngr.name,
	rmsngr.requests,
	rmsngr.responses,
	rmsngr.nameSpace,
	rmsngr.traceMonitor
      )
    }
}

class SociallyAwareMessenger[ReqBody,RspBody](
  //handle : URI,
  handle : Moniker,
  monitor : TraceMonitor[ReqBody,RspBody]
) extends ReflectiveMessenger[ReqBody,RspBody](
  handle,
  new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
  new ListBuffer[JustifiedResponse[ReqBody,RspBody]](),
  //Some( new LinkedHashMap[URI,Socialite[ReqBody,RspBody]]() ),
  Some( new LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]() ),
  monitor
  ){    
}

// Messenger class that handles messaging with continuations

class RMessenger[ReqBody,RspBody](
  //name : URI,
  name : Moniker,
  requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  //nameSpace : Option[LinkedHashMap[URI,Socialite[ReqBody,RspBody]]],
  nameSpace : Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]],
  traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends ReflectiveMessenger[ReqBody,RspBody](
  name, requests, responses, nameSpace, traceMonitor
) {
}
