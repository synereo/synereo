// -*- mode: Scala;-*- 
// Filename:    SpecialKFilter.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:13:20 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.http.servlet

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.util.continuations._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable._

import java.net.URI
import java.util.UUID

import javax.servlet._
import javax.servlet.{ServletRequest => HSReq, ServletResponse => HSRsp}
import javax.servlet.http.HttpUtils
import javax.servlet.http.{HttpServlet}
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

object MonadicHttpTS
 extends MonadicTermStoreScope[Symbol,Symbol,Any,HTTPRequestCtxt]
  with HTTPTrampolineScope[Symbol,Symbol,Any]
  with UUIDOps
{
  import SpecialKURIDefaults._
  import CnxnLeafAndBranch._
  import CCLDSL._

  val HTTPReqCCL =
    $('request)(
      // taken from SWI Prolog http interface
      $('host)('Host),
      $('input)('IStrm),
      $('method)('Method),
      $('path)('Path),
      $('peer)('Peer),
      $('port)('Port),
      $('requestURI)('ReqURI),
      $('query)('Query),
      $('httpVersion)('HttpVersion),
      $('cookie)('Cookie),
      // added from Java HttpServletRequest spec
      $('parts)('Parts)
    )

  type MTTypes = MonadicTermTypes[Symbol,Symbol,Any,HTTPRequestCtxt]
  object TheMTT extends MTTypes
  override def protoTermTypes : MTTypes = TheMTT

  type DATypes = DistributedAskTypes
  object TheDAT extends DATypes
  override def protoAskTypes : DATypes = TheDAT        
  
  override type HTTPTrampolineTypes = 
    HTTPTrampoline[Symbol,Symbol,Any]
    with DTSMsgScope[Symbol,Symbol,Any,HTTPRequestCtxt]

  object HTTPHobo
    extends HTTPTrampoline[Symbol,Symbol,Any]
    with DTSMsgScope[Symbol,Symbol,Any,HTTPRequestCtxt]
  {
    override type MsgTypes = DTSMSH[Symbol,Symbol,Any,HTTPRequestCtxt]       
    object MonadicDMsgs extends MsgTypes {   
      val protoDreqUUID = getUUID()
      val protoDrspUUID = getUUID()    
        
      override def protoDreq : DReq =
	MDGetRequest( $('protoDReq)( "yo!" ) )
      override def protoDrsp : DRsp =
	MDGetResponse( $('protoDRsp)( "oy!" ), null )
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
    
    override type HTTPConverter = HTTPToCnxnCtxtLabel[Symbol,Symbol,Any]
    
    object HTTPNonCoverter extends HTTPConverter {
      def asCall(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[Symbol,Symbol,Any,HTTPRequestCtxt]]
      = { 
	val req = hctxt.req
	Some(
	  Msgs.MDPutRequest(
	    $('request)(
	      // taken from SWI Prolog http interface
	      $('host)( req.getServerName ),
	      $('input)( req.getReader ),
	      $('method)( req.getMethod ),
	      $('path)( req.getPathTranslated ),
	      $('peer)('Peer),
	      $('port)( req.getServerPort.toString ),
	      $('requestURI)( req.getRequestURI ),
	      $('query)( req.getQueryString ),
	      $('httpVersion)('HttpVersion),
	      $('cookie)( req.getCookies ),
	      // added from Java HttpServletRequest spec
	      $('parts)('Parts)
	    ),
	    hctxt
	  )
	)
      }
      def rspPickupLoc(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[Symbol,Symbol,Any,HTTPRequestCtxt]]
      =
	None
      def asCCL(
	hctxt : HTTPRequestCtxt
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] =
	None
      def asCCL(
	hsrq : HttpServletRequest
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] =
	None
      def asCCL(
	hsrq : HttpServletResponse
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] =
	None
    }
    
    override def protoHTTPConverter = HTTPNonCoverter
  }

  override def protoHttpTramp = HTTPHobo

  override type MsgTypes = httpTramp.MsgTypes
  override def protoMsgs = httpTramp.protoMsgs  
  
  lazy val Mona = new MonadicTermStore()
}




