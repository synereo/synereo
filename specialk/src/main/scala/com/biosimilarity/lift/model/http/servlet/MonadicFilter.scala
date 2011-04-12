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

class MonadicFilter
extends Filter with Journalist with UUIDOps {
  import MonadicHttpTS._
  import mTT._
  override def init( fltrCfg : FilterConfig ) = {
    // TBD
  }  
  
  lazy val mySpace = loopBack()

  override def doFilter(
    req : HSReq, rsp : HSRsp, chain : FilterChain
  ) = {
    ( req, rsp ) match {
      case ( hsrq : HttpServletRequest, hsrp : HttpServletResponse ) => {
	// tweet( " we are here! " )
// 	hsrp.setContentType("text/html")
// 	val pw : java.io.PrintWriter = hsrp.getWriter()
// 	pw.println("<html>")
// 	pw.println("<head><title>Hello World</title></title>")
// 	pw.println("<body>")
// 	pw.println("<h1>Hello World</h1>")
// 	pw.println("</body></html>")

	val hctxt = HTTPRequestCtxt( hsrq, hsrp, chain, Some( getUUID ) )
	for( call <- httpTramp.httpConverter.asCall( hctxt ) ) {
	  call match {	    
	    //Calculating a ptn at this point affords redirect semantics
	    case httpTramp.Msgs.MDPutRequest( ptn, v ) => {
	      reset {	    
		mySpace.put( ptn, Ground( v ) )
		httpTramp.httpConverter.rspPickupLoc( hctxt ) match {
		  case Some( replyLocWrapper ) => {
		    replyLocWrapper match {
		      case httpTramp.Msgs.MDPutRequest(
			rplyLoc, nhctxt
		      ) => {
			for( rsrc <- mySpace.getValue( rplyLoc ) ) {
			  tweet( "should serve up: " + rsrc )
			  val response = rsrc.resp		    
			  response.setContentType("text/html")
			  val pw : java.io.PrintWriter = response.getWriter()
			  pw.println("<html>")
			  pw.println("<head><title>Hello World</title></title>")
			  pw.println("<body>")
			  pw.println("<h1>Hello World</h1>")
			  pw.println("</body></html>")
			}
		      }
		    }
		  }
		  case None => {
		    tweet(
		      "What to do if there is no reply location?"
		    );
		  }
		}
	      }
	      //chain.doFilter( req, rsp )
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


