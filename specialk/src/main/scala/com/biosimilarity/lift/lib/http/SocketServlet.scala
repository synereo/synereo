// -*- mode: Scala;-*- 
// Filename:    SocketServlet.scala<2> 
// Authors:     lgm                                                    
// Creation:    Fri Dec  2 14:47:34 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.websocket

import scala.collection.mutable.Queue

import javax.servlet.http.HttpServletRequest

import java.io.IOException;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.eclipse.jetty.websocket.WebSocketServlet
import org.eclipse.jetty.websocket.WebSocket
import java.net.URI
import com.biosimilarity.lift.lib.kvdbJSON.KVDBJSONAPIDispatcher

/**

GLENandGREG need to talk through if the client sets a UUID in the URI or just puts UUID's in each message.  
I (aka Glen) am not clear if more framing is needed.  I.e. I think we send the framing in each message from the 
client.  We "could" have the framing at the socket level and send it once as well then all messages get 
that from URI, etc, etc...  Anyways this is enough text to trigger my memory for discussion

GLENandGREG when should serveAPI be called?  Per web socket connection?  That is my best educated 2 cent guess ;-)

*/
object SocketServlet {
  lazy val dispatcher = {
    val dsp =
      new KVDBJSONAPIDispatcher( new URI( "amqp", "localhost", "/kvdb", "" ) )

    val trgtURI = 
      new URI( "agent", "localhost", "/kvdbDispatchStore1", "" )

    val srcURI = 
      new URI( "agent", "localhost", "/kvdbDispatchStore2", "" )  
      
    // Configure it to serve requests with a "to" header of agent://localhost/kvdbDispatchStore1
    // and a "from" header of agent://localhost/kvdbDispatchStore2
    // and deposit replies to the kvdbReplyQueue in the kvdbReplyExchange  
    dsp.addSingletonKVDB( trgtURI )
    // dsp.addReplyQueue( srcURI, "localhost", "kvdbReply" )
    dsp.serveAPI
    
    dsp
  }
  
}

class SocketServlet extends org.eclipse.jetty.websocket.WebSocketServlet {
  
  import SocketServlet.dispatcher
   
  def doWebSocketConnect(
    request : HttpServletRequest,
    protocol : String
  ) : WebSocket = {
    val uri = new URI(request.getParameter("uri"))
    println("socket received " + request)
    QueuingWebSocket( 
      dispatcher
      , uri
    )
  }  
}
