// -*- mode: Scala;-*- 
// Filename:    SpecialKServlet.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 12:18:05 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.http.servlet

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

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

class SpecialKServlet
extends HttpServlet
with MsgShortHand {
  lazy val _msngrMoniker = IdVendor.getURI()
  case object TheSpecialK extends SpecialKMessenger( _msngrMoniker )

  override def init(){
    // Teach TheSpecialK it's name
    TheSpecialK.introduce( _msngrMoniker, TheSpecialK )
    // Open a monitoring session
    AnHTTPTraceMonitor.openMonitoringSession( TheSpecialK )
    
    // Engage TheSpecialK in handling messages
    TheSpecialK.start

    // Engage AnHTTPTraceMonitor
    AnHTTPTraceMonitor.start
  }  
  
  override def doGet(req : HttpSReq, resp : HttpSResp)
  {
    // Wrap the request/response pair in a Justified structure, then
    // send the pair to TheSpecialK
    TheSpecialK ! wrapHSReqRespPair( req, resp )

    // Some default behavior
    resp.setContentType("text/html")
    val out = resp.getWriter()
    out.println( "Hello World! <br />" )
  }
}

