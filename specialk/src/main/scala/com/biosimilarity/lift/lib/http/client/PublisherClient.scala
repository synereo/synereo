package com.biosimilarity.lift.lib.http.client

object PublisherClient extends BaseClientApp {
    
  def run = {
    (1 to 5) foreach { i =>
      sendPublish("nomnom", "msg #" + i)
    }
  }

}
