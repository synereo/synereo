// -*- mode: Scala;-*- 
// Filename:    monadic-www.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 28 08:38:28 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib._
import scala.util.continuations._
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

object WWW extends MonadicEmbeddedJetty[String] with WireTap {
  override def tap [A] ( fact : A ) : Unit = { BasicLogService.reportage( fact ) }

  def fillInResponse( request : HttpServletRequest, response : HttpServletResponse ) : Unit = {
    response.setContentType( "text/html" )
    response.setStatus( HttpServletResponse.SC_OK )
    response.getWriter().println(
      (
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 " +
        "Transitional//EN\">\n" +
        "<HTML>\n" +
        "<HEAD><TITLE>Hello WWW</TITLE></HEAD>\n" +
        "<BODY>\n" +
        "<H1>Hello WWW</H1>\n" +
	"session = " + request.getSession( true ).getId() +
        "</BODY></HTML>"
      )
    )    
  }

  def run() : Unit = {
    reset {
      for( reqRsp <- beginService( 8090, "/" ) ) {
	val request = reqRsp.httpServletReq
	val response = reqRsp.httpServletRsp

	println( "serving request: " + request )

	fillInResponse( request, response )
      }
    }
  }
}
