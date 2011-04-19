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
 extends MonadicTermStoreScope[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]
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

  type MTTypes =
    MonadicTermTypes[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]
  object TheMTT extends MTTypes
  override def protoTermTypes : MTTypes = TheMTT

  type DATypes = DistributedAskTypes
  object TheDAT extends DATypes
  override def protoAskTypes : DATypes = TheDAT        
  
  override type HTTPTrampolineTypes = 
    HTTPTrampoline[Symbol,Symbol,Any]
    with DTSMsgScope[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]

  object HTTPHobo
    extends HTTPTrampoline[Symbol,Symbol,Any]
    with DTSMsgScope[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]
  {
    override type MsgTypes =
      DTSMSH[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]       
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
    
    object HTTPBasicConverter extends HTTPConverter {
      def asCall(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]]
      = { 	
	for( ccl <- asCCL( hctxt ) ) 
	  yield {
	    Msgs.MDPutRequest( ccl, ccl )
	  }
      }
      def rspPickupLoc(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[Symbol,Symbol,Any,CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]]
      = {
	for( cnvId <- hctxt.conversationId )
	  yield {
	    val rspCCL = 
	      ?('response)(
		// taken from SWI Prolog http interface
		?('host)( 'Host ),
		?('input)( 'IStream ),
		?('method)( 'Method ),
		?('path)( 'Path ),
		?('peer)('Peer),
		?('port)( 'Port ),
		?('hsrquestURI)('RequestURI ),
		?('query)( 'Query ),
		?('httpVersion)('HttpVersion),
		?('cookie)( 'Cookie ),
		// added from Java HttpServletRequest spec
		?('parts)('Parts),
		// unique to this implementation
		?('conversationId)( cnvId.toString )
	      )
	    Msgs.MDPutRequest( rspCCL, rspCCL )
	  }
      }
      def asCCL(
	hctxt : HTTPRequestCtxt
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] = {
	for(
	  cnvId <- hctxt.conversationId;
	  ccl <- asCCL( hctxt.req, cnvId )
	) yield { ccl }
      }
      def asCCL(
	hsrq : HttpServletRequest,
	cnvId : UUID
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] = {
	Some(
	  ?('request)(
	      // taken from SWI Prolog http interface
	    ?('host)( hsrq.getServerName ),
	    ?('input)( hsrq.getReader.toString ),
	    ?('method)( hsrq.getMethod ),
	    ?('path)( hsrq.getPathTranslated ),
	    ?('peer)('Peer),
	    ?('port)( hsrq.getServerPort.toString ),
	    ?('hsrquestURI)( hsrq.getRequestURI.toString ),
	    ?('query)( hsrq.getQueryString ),
	    ?('httpVersion)('HttpVersion),
	    ?('cookie)( (hsrq.getCookies + "") ),
	    // added from Java HttpServletRequest spec
	    ?('parts)('Parts),
	    // unique to this implementation
	    ?('conversationId)( cnvId.toString )
	  )
	)
      }
      def asCCL(
	hsrq : HttpServletResponse,
	cnvId : UUID
      ) : Option[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual] =
	None
    }
    
    override def protoHTTPConverter = HTTPBasicConverter
  }

  override def protoHttpTramp = HTTPHobo

  override type MsgTypes = httpTramp.MsgTypes
  override def protoMsgs = httpTramp.protoMsgs  
  
  lazy val Mona = new MonadicTermStore()
  def ptToPt( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )    
    def loopBack() = {
      ptToPt( "localhost", "localhost" )
    }
}




