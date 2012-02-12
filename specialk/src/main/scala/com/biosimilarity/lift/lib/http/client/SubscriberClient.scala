package com.biosimilarity.lift.lib.websocket.client

object SubscriberClient extends BaseClientApp {

  def run = {
    sendSubscribe("nomnom")
    println("waiting 120 seconds for messages")
    wait(120)
  }

}