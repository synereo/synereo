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

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

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
//import javax.servlet.annotation

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

object SpecialKMonadicServletDefaults {
  implicit val asyncTimeout = 1000
}

// @WebServlet("/monadic", asyncSupported=true)
// class SpecialKMonadicServlet
// extends HttpServlet
// with MonadicDispatcher[T] 
// with MsgShortHand 
// with WireTap
// with Journalist {
//   import SpecialKMonadicServletDefaults._
//   lazy val _msngrMoniker = IdVendor.getURI()

//   type ConnectionParameters = AsyncContext
//   type Channel = URI
//   type Ticket = AsyncContext
//   type Payload = HttpPair

//   def reqBodyStr( req : HttpSReq ) = {
//     val reqIStrm = req.getInputStream
//     val in =
//       new ObjectInputStream( reqIStrm )
//     in.readObject.asInstanceOf[String]
//   }

//   override def callbacks( channel : Channel, ticket : Ticket) =
//     Generator {
//       k : ( Payload => Unit @suspendable) =>

//       reportage("level 1 callbacks")

//       shift {
// 	outerk : (Unit => Any) =>
	  
// 	  class Rendezvous()
// 	   extends AsyncListener {
//     	     override def onStartAsync(
// 	       event : AsyncEvent
// 	     ) {
//     		 spawn { 
//   		   reportage("before continuation in callback")
  		
//     		   k(
// 		     (
// 		       event.getSuppliedRequest,
// 		       event.getSuppliedResponse
// 		     )
// 		   )
    		
//     		   reportage("after continuation in callback")
    		   
// 		   outerk()
//     		 }
//     	     }
// 	     override def onError(
// 	       event : AsyncEvent 
// 	     ) {
// 	     }
// 	     override def onComplete(
// 	       event : AsyncEvent 
// 	     ) {
// 	     }
// 	     override def onTimeout(
// 	       event : AsyncEvent 
// 	     ) {
// 	     }
// 	   }
  	
//   	reportage("before registering callback")
  	
// 	ticket.addListener( new Rendezvous() )
  	
//   	reportage("after registering callback")
//   	// stop
//       }
//     }

//   def readT( channel : Channel, ticket : Ticket ) =
//     Generator {
//       k: ( T => Unit @suspendable) =>
// 	shift {
// 	  outerk: (Unit => Unit) =>
// 	    reset {
	      
//   	      for (
// 		httpPayload <- callbacks( channel, ticket )
// 	      )	{
		
// 		val ( req, resp ) = httpPayload				

// 		val t =
// 		  req.getContentType() match {
// 		    case "application/octet-stream" => {
// 		      val xstrm =
// 			new XStream( new JettisonMappedXmlDriver() )
// 		      xstrm.fromXML( reqBodyStr( req ) ).asInstanceOf[T]
// 		    }
// 		    case "application/json" => {
// 		      val xstrm =
// 			new XStream( new JettisonMappedXmlDriver() )
// 		      xstrm.fromXML( reqBodyStr( req ) ).asInstanceOf[T]
// 		    }
// 		    case "text/xml" => {
// 		      val xstrm = new XStream( )
// 		      xstrm.fromXML( reqBodyStr( req ) ).asInstanceOf[T]
// 		    }
// 		  }

// 		k( t )
		
// 		ticket.complete()

// 		// Is this necessary?
// 		shift { k : ( Unit => Unit ) => k() }
//   	      }
  	  
//   	      reportage( "readT returning" )
//   	      outerk()
// 	    }
// 	}
//     }

//   override def init(){
//     // ???
//   }  
  
//   override def doGet(req : HttpSReq, resp : HttpSResp)
//   {
//     val httpPair = ( req, resp )
//     val asyncCtxt = req.startAsync( req, resp )

//     resp.setAsyncTimeout( asyncTimeout )
//   }
// }

