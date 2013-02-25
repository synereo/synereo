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

      def handleMessage(msg: Message) =
      {
        println("received: " + msg)
      }

      val exchange = "mult"
      val exchangeContent = "123content(_)"
      val exchangeSearch = "search"
      val routingKey = "routeroute"
      val config = new RabbitConfiguration("localhost", 5672, "kvdb", "anywhere")
      val default = new MessageAMQPListener(config, exchange, routingKey, handleMessage(_: Message))
      val content = new MessageAMQPListener(config, exchangeContent, routingKey, handleMessage(_: Message))
      val search = new MessageAMQPListener(config, exchangeSearch, routingKey, handleMessage(_: Message))
      for ( i <- 1 to 10000 ) {
        val a = new MessageAMQPPublisher(config, exchangeContent, routingKey)
        a.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }
      Thread.sleep(10000)
      success
    }
  }

  "basicPublish" should {
    "be found by basicConsume" in {

      def handleMessage(msg: Message) =
      {
        println("received: " + msg)
      }

      val exchange = "mult"
      val exchangeContent = "123content(_)"
      val exchangeSearch = "search"
      val exchangeRandom = "random"
      val routingKey = "routeroute"
      val config = new RabbitConfiguration("localhost", 5672, "kvdb", "anywhere")

      val default = new MessageAMQPListener(config, exchange, routingKey, handleMessage(_: Message))
      val content = new MessageAMQPListener(config, exchangeContent, routingKey, handleMessage(_: Message))
      val search = new MessageAMQPListener(config, exchangeSearch, routingKey, handleMessage(_: Message))
      for ( i <- 1 to 1000 ) {
        val a = new MessageAMQPPublisher(config, exchangeContent, routingKey)
        a.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }
      for ( i <- 1 to 1000 ) {
        val b = new MessageAMQPPublisher(config, exchangeSearch, routingKey)
        b.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }

      //no consumer on this one
      for ( i <- 1 to 1000 ) {
        val c = new MessageAMQPPublisher(config, exchangeRandom + i, routingKey)
        c.send(new GetContentRequest(new EventKey(UUID.randomUUID(), i.toString), Profile.SEARCH_ALL))
      }
      Thread.sleep(10000)
      success
    }
  }
}
