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

class SocketServlet extends org.eclipse.jetty.websocket.WebSocketServlet {
  def doWebSocketConnect(
    request : HttpServletRequest,
    protocol : String
  ) : WebSocket = {    
    QueuingWebSocket( 
      theWSMgr,
      new Queue[String]( )
    )
  }  
}

// class KVDBSocketServlet extends org.eclipse.jetty.websocket.WebSocketServlet {
//   def doWebSocketConnect(
//     request : HttpServletRequest,
//     protocol : String
//   ) : WebSocket = {    
//     KVDBWebSocket( 
//       theKVDBWSMgr,
//       request
//     )
//   }  
// }
