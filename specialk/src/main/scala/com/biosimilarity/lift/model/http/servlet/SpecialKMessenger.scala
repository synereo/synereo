// -*- mode: Scala;-*- 
// Filename:    SpecialKMessenger.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 13:58:10 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.http.servlet

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import scala.collection.mutable._

import java.net.URI
import java.util.UUID

import javax.servlet._
import javax.servlet.{ServletRequest => HSReq, ServletResponse => HSResp}
import javax.servlet.http.HttpUtils
import javax.servlet.http.{HttpServlet
			   , HttpServletRequest => HttpSReq
			   , HttpServletResponse => HttpSResp
			 }

trait MsgShortHand {
  import identityConversions._
  type HttpPair = ( HSReq, HSResp )
  type JHttpReq = JustifiedRequest[HttpPair,HSResp]
  type JHttpRsp = JustifiedResponse[HttpPair,HSResp]

  object IdVendor extends UUIDOps {
    def getURI() = {
      new URI( "com", "biosimilarity", getUUID().toString )
    }
  }

  object AnHTTPTraceMonitor extends TraceMonitor[HttpPair,HSResp]

  def source(req : HSReq, resp : HSResp)
  : //URI = {
  Moniker = {
    // temporarily
    IdVendor.getURI()
  }
  def target(req : HSReq, resp : HSResp)
  : //URI = {
  Moniker = {
    // temporarily
    req match {
      case hsReq : HttpSReq => {
	new URI( hsReq.getRequestURI() )
      }
      case _ => {
	IdVendor.getURI()
      }
    }
  }
  def label(req : HSReq, resp : HSResp)
  : UUID = {
    // temporarily
    IdVendor.getUUID()
  }

  def wrapHSReqRespPair(req : HSReq, resp : HSResp)
  : JHttpReq = {
    JustifiedRequest(
      IdVendor.getUUID(),
      target( req, resp ),
      source( req, resp ),
      label( req, resp ),
      ( req, resp ),
      None
    )
  }

  class SpecialKMessenger(
    //alias : URI
    alias : Moniker
  )
  extends ReflectiveMessenger[HttpPair,HSResp](
    alias,
    new ListBuffer[JHttpReq](),
    new ListBuffer[JHttpRsp](),
    //Some( new LinkedHashMap[URI,Socialite[HttpPair,HSResp]]),
    Some( new LinkedHashMap[Moniker,Socialite[HttpPair,HSResp]]),
    AnHTTPTraceMonitor
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
      request : JHttpReq,
      k : Status[JHttpReq] => Status[JHttpReq]
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
