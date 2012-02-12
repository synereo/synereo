package com.biosimilarity.lift.lib.websocket.client


object GetPutClient extends BaseClientApp {

  def run = {
    sendPut ("helloWorld", "The Hello World Value")
    sendGet("helloWorld")
  }

}
