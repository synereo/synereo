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
 extends MonadicTermStoreScope[String,String,String,HTTPRequestCtxt]
  with HTTPTrampolineScope[String,String,String]
  with UUIDOps
{
  import SpecialKURIDefaults._
  import CnxnLeafAndBranch._

  type MTTypes = MonadicTermTypes[String,String,String,HTTPRequestCtxt]
  object TheMTT extends MTTypes
  override def protoTermTypes : MTTypes = TheMTT

  type DATypes = DistributedAskTypes
  object TheDAT extends DATypes
  override def protoAskTypes : DATypes = TheDAT        
  
  override type HTTPTrampolineTypes = 
    HTTPTrampoline[String,String,String]
    with DTSMsgScope[String,String,String,HTTPRequestCtxt]

  object HTTPHobo
    extends HTTPTrampoline[String,String,String]
    with DTSMsgScope[String,String,String,HTTPRequestCtxt]
  {
    override type MsgTypes = DTSMSH[String,String,String,HTTPRequestCtxt]       
    object MonadicDMsgs extends MsgTypes {   
      val protoDreqUUID = getUUID()
      val protoDrspUUID = getUUID()    
        
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, null )
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
    
    override type HTTPConverter = HTTPToCnxnCtxtLabel[String,String,String]
    
    object HTTPNonCoverter extends HTTPConverter {
      def asCall(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[String,String,String,HTTPRequestCtxt]]
      = None
      def rspPickupLoc(
	hctxt : HTTPRequestCtxt
      ) :
      Option[Msgs.MDistributedTermSpaceRequest[String,String,String,HTTPRequestCtxt]]
      =
	None
      def asCCL(
	hctxt : HTTPRequestCtxt
      ) : Option[CnxnCtxtLabel[String,String,String] with Factual] =
	None
      def asCCL(
	hsrq : HttpServletRequest
      ) : Option[CnxnCtxtLabel[String,String,String] with Factual] =
	None
      def asCCL(
	hsrq : HttpServletResponse
      ) : Option[CnxnCtxtLabel[String,String,String] with Factual] =
	None
    }
    
    override def protoHTTPConverter = HTTPNonCoverter
  }

  override def protoHttpTramp = HTTPHobo

  override type MsgTypes = httpTramp.MsgTypes
  override def protoMsgs = httpTramp.protoMsgs  
  
  lazy val Mona = new MonadicTermStore()
}

class MonadicFilter
extends Filter with Journalist {
  import MonadicHttpTS._
  import mTT._
  override def init( fltrCfg : FilterConfig ) = {
    // TBD
  }  
  
  override def doFilter(
    req : HSReq, rsp : HSRsp, chain : FilterChain
  ) = {
    ( req, rsp ) match {
      case ( hsrq : HttpServletRequest, hsrp : HttpServletResponse ) => {
	val hctxt = HTTPRequestCtxt( hsrq, hsrp, chain )
	for( call <- httpTramp.httpConverter.asCall( hctxt ) ) {
	  call match {	    
	    // Calculating a ptn at this point affords redirect semantics
	    case httpTramp.Msgs.MDPutRequest( ptn, v ) => {
	      reset {	    
		Mona.put( ptn, Ground( v ) )
		for( rsrc <- httpTramp.httpConverter.rspPickupLoc( hctxt ) ) {
		  tweet( "should serve up: " + rsrc )
		}
	      }
	      chain.doFilter( req, rsp )
	    }
	    case _ => {
	      tweet(
		"What are the semantics of other request types?"
	      )
	      chain.doFilter( req, rsp )
	    }
	  }
	}
      }
    }
  }
  
  override def destroy() {
  }
}


