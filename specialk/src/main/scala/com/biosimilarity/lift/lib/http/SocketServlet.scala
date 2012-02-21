// -*- mode: Scala;-*- 
// Filename:    SocketServlet.scala<2> 
// Authors:     lgm                                                    
// Creation:    Fri Dec  2 14:47:34 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.http

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse;
import java.net.URI
import com.biosimilarity.lift.lib.kvdbJSON.KVDBJSONAPIDispatcher
import org.eclipse.jetty.websocket.WebSocket

/**

GLENandGREG need to talk through if the client sets a UUID in the URI or just puts UUID's in each message.  
I (aka Glen) am not clear if more framing is needed.  I.e. I think we send the framing in each message from the 
client.  We "could" have the framing at the socket level and send it once as well then all messages get 
that from URI, etc, etc...  Anyways this is enough text to trigger my memory for discussion

*/
object SocketServlet {

  val requestQueue = new RequestQueue {
    val delegate = new java.util.concurrent.LinkedBlockingQueue[String]()
    def hasNext = true
    def next = delegate.take // this is a blocking call
    def put(msg: String) = delegate.put(msg)    
    override def size = delegate.size  // important otherwise a call to size blocks because it iterates
  }

  lazy val dispatcher = {
    val dsp =
      new KVDBJSONAPIDispatcher( new URI( "amqp", "localhost", "/kvdb", "" ) )

    val trgtURI = 
      new URI( "agent", "localhost", "/kvdbDispatchStore1", "" )

    // val srcURI = 
    //   new URI( "agent", "localhost", "/kvdbDispatchStore2", "" )  
      
    dsp.addSingletonKVDB( trgtURI )
    
    // # of dispatcher worker threads we want    
    val workerCount = 1
    
    (1 to workerCount) foreach { i =>
      new Thread {
        setName("socketServletRequestQueue-" + i)
        override def run = {
          dsp.serveAPI(requestQueue)
        }
      }.start
    }
    
    dsp
  }
  
}

class SocketServlet extends org.eclipse.jetty.websocket.WebSocketServlet {
  
  import SocketServlet._
   
  def doWebSocketConnect(
    request : HttpServletRequest,
    protocol : String
  ) : WebSocket = {
    val uri = new URI(request.getParameter("uri"))
    println("socket received " + request)
    QueuingWebSocket(
      requestQueue,
      ( scp : SocketConnectionPair ) => {
	dispatcher.socketURIMap += ( uri -> scp )
      },
      () => { dispatcher.socketURIMap -= uri }
    )
  }  
}
