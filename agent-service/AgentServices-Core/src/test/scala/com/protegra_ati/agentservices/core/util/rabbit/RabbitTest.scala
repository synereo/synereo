package com.protegra_ati.agentservices.core.util.rabbit

import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import com.protegra_ati.agentservices.core.messages.content.{GetContentRequest, DeleteContentRequest}
import org.specs2.mutable._
import org.specs2.time.Duration
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.concurrent.atomic.AtomicInteger
import concurrent.ThreadPoolRunnersX

class RabbitTest
  extends SpecificationWithJUnit
  with ThreadPoolRunnersX
{
  sequential

  "basicPublish" should {
    "be found by basicConsume" in {
      val messageCount = new AtomicInteger(0)
      def handleMessage(msg: Message) =
      {
        messageCount.incrementAndGet()
      }

      val exchangeSearch = "search"
      val routingKey = "routeroute"
      val config = new RabbitConfiguration("localhost", 5672, "kvdb", "anywhere")
      val search = new MessageAMQPListener(config, exchangeSearch, routingKey, handleMessage(_: Message))

      val numMessages = 10000
      for ( i <- 1 to numMessages ) {
        spawn {
          MessageAMQPPublisher.sendToRabbit(config, exchangeSearch, routingKey, new DeleteContentRequest())
        }
      }
      messageCount.get() must be_==(numMessages).eventually(60, new Duration(1000))
      success
    }
  }

  "basicPublish" should {
    "be found by basicConsume" in {
      val contentMessageCount = new AtomicInteger(0)
      val searchMessageCount = new AtomicInteger(0)

      def handleContentMessage(msg: Message) = {
        contentMessageCount.incrementAndGet()
      }
      def handleSearchMessage(msg: Message) = {
        searchMessageCount.incrementAndGet()
      }

      val exchangeContent = "123content(_)"
      val exchangeSearch = "search_2"
      val exchangeRandom = "random_2"
      val routingKey = "routeroute"
      val config = new RabbitConfiguration("localhost", 5672, "kvdb", "anywhere")

      val content = new MessageAMQPListener(config, exchangeContent, routingKey, handleContentMessage(_: Message))
      val search = new MessageAMQPListener(config, exchangeSearch, routingKey, handleSearchMessage(_: Message))

      val numMessages = 1000
      for ( i <- 1 to numMessages ) {
        spawn {
          MessageAMQPPublisher.sendToRabbit(config, exchangeContent, routingKey, new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
        }
      }

      for ( i <- 1 to numMessages ) {
        spawn {
          MessageAMQPPublisher.sendToRabbit(config, exchangeSearch, routingKey, new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
        }
      }

      //no consumer on this one
      for ( i <- 1 to numMessages ) {
        spawn {
          MessageAMQPPublisher.sendToRabbit(config, exchangeRandom + i, routingKey, new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
        }
      }

      contentMessageCount.get() must be_==(numMessages).eventually(60, new Duration(1000))
      searchMessageCount.get() must be_==(numMessages).eventually(60, new Duration(1000))

      success
    }
  }
}
