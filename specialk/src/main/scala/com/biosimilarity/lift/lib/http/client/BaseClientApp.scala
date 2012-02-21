package com.biosimilarity.lift.lib.http.client


import java.net.URI
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.TimeUnit
import java.util.UUID

import org.eclipse.jetty.websocket.WebSocket;
import org.eclipse.jetty.websocket.WebSocketClientFactory;

trait BaseClientApp extends App {

//  lazy val socketServerUrl = new URI("ws://127.0.0.1:8090/websocket")
  lazy val clientUri = "websocket://blah/blah/" + UUID.randomUUID.toString

  lazy val socketServerUrl = {
    val uri = new URI("ws://127.0.0.1:8080/websocket?uri=" + clientUri)
    println("server url is " + uri)
    uri
  }

  lazy val client = {
    val factory = new WebSocketClientFactory()
    factory.start
    val client = factory.newWebSocketClient
    client.setMaxIdleTime(30000)
    client.setProtocol("chat")
    client
  }
  lazy val connection = {
    client.open(socketServerUrl, new WebSocket.OnTextMessage {
         def onOpen(conn: WebSocket.Connection) = println("-- open")
         def onClose(closeCode: Int, msg: String) = println("-- close " + closeCode + " -- " + msg)
         def onMessage(msg: String) = println("-- received message: " + msg)
    }).get(5, TimeUnit.SECONDS)
  }  
  
  lazy val messagesReceivedCounter = new AtomicInteger

  lazy val waiter = new Object
  def wait(seconds: Int) = waiter.synchronized(waiter.wait(seconds*1000))

  def sendGet(key: String) = send("get", key)
  def sendPut(key: String, value: String) = send("put", key, value)
  def sendSubscribe(key: String) = send("subscribe", key)
  def sendPublish(key: String, value: String) = send("publish", key, value)
  
  private def send(verb: String, key: String, value: String = "") = {
    val uuid = UUID.randomUUID.toString
    val msg = 
""" 
{
      "uid": ":uid:"
      , "verb": ":verb:"
      , "key": ":key:"
      , "value": ":value:"
}      
""".replace(":uid:", uuid)
   .replace(":verb:", verb)
   .replace(":key:", key)
   .replace(":value:", value)
   .replace(":clientUri:",clientUri)
    println("--sending message: " + msg);
    rawSend(msg)
  }
  
  def rawSend(msg: String) =  connection sendMessage msg
  
  def run
  
  try {
    run
  } finally {
    connection.disconnect()
  }
  
}
