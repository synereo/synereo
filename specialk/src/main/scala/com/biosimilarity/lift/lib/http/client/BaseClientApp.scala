package com.biosimilarity.lift.lib.websocket.client


import java.net.URI
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.TimeUnit
import java.util.UUID

import org.eclipse.jetty.websocket.WebSocket;
import org.eclipse.jetty.websocket.WebSocketClientFactory;

trait BaseClientApp extends App {

  val url = new URI("ws://127.0.0.1:8090/querysocket")

  lazy val client = {
    val factory = new WebSocketClientFactory()
    factory.start
    val client = factory.newWebSocketClient
    client.setMaxIdleTime(30000)
    client.setProtocol("chat")
    client
  }
  lazy val connection = {
    client.open(url, new WebSocket.OnTextMessage {
         def onOpen(conn: WebSocket.Connection) = println("-- open")
         def onClose(closeCode: Int, msg: String) = println("-- close " + closeCode + " -- " + msg)
         def onMessage(msg: String) = println("-- received message: " + msg)
    }).get(5, TimeUnit.SECONDS)
  }  
  
  val messagesReceivedCounter = new AtomicInteger

  val waiter = new Object
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
    println("--sending message: " + msg);
    connection sendMessage msg
  }
  
  def run
  
  try {
    run
  } finally {
    connection.disconnect()
  }
  
}
