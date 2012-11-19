package com.protegra_ati.agentservices.core.util.rabbit

import org.specs2.mutable._

import java.util.UUID
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.core.messages.{Message, EventKey}
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import scala.concurrent.ops._

class RabbitTest extends SpecificationWithJUnit
{
  "basicPublish" should {
    "be found by basicConsume" in {

      def handleMessage(msg: Message) = {
        System.err.println("received: " + msg)
      }

      val exchange = "mult"
      val exchangeContent = "content(_)"
      val exchangeSearch = "search"
      val routingKey = "routeroute"

      val default = new MessageAMQPListener("localhost", 5672, exchange, routingKey, handleMessage(_: Message))
      val content = new MessageAMQPListener("localhost", 5672, exchangeContent, routingKey, handleMessage(_: Message))
      val search = new MessageAMQPListener("localhost", 5672, exchangeSearch, routingKey, handleMessage(_: Message))
      val a = new MessageAMQPPublisher("localhost", 5672, exchangeContent, routingKey)
      val b = new MessageAMQPPublisher("localhost", 5672, exchangeSearch, routingKey)
      for (i <- 1 to 100)
      {
          a.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }
      for (i <- 1 to 10)
      {
          b.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }
      Thread.sleep(1000)
      Thread.sleep(1000)
      success
    }
  }
}
