package com.biosimilarity.lift.lib

import org.scalatest.{Matchers, WordSpec}

class AMQPURIOpsSpec extends WordSpec with Matchers {

  import java.net.URI

  // AMQPURIOps is a trait
  object AMQPURIOpsInstance extends AMQPURIOps
  import AMQPURIOpsInstance._

  val testURI: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=routeroute", null)

  "uriHost" should {
    "return the host of an instance of URI" in {
      assert(uriHost(testURI) == "localhost")
    }
  }

  "uriPort" should {
    "return the port of an instance of URI" in {
      assert(uriPort(testURI) == 5672)
    }
  }

  "uriExchange" should {

    "return the default exchange for an instance of URI containing no exchange" in {
      val testURINoExchange: URI = new URI("amqp", null, "localhost", 5672, null, null, null)
      assert(uriExchange(testURINoExchange) == "mult")
    }

    "return the exchange of an instance of URI" in {
      assert(uriExchange(testURI) == "mult")
    }

    "return the exchange of an instance of URI (2)" in {
      val testURIExchange02: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult", "routingKey=routeroute", null)
      assert(uriExchange(testURIExchange02) == "mult")
    }

    "return the exchange of an instance of URI (3)" in {
      val testURIExchange03: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult/iult", "routingKey=routeroute", null)
      assert(uriExchange(testURIExchange03) == "mult")
    }

    "return the exchange of an instance of URI (4)" in {
      val testURIExchange04: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult/iult/gult", "routingKey=routeroute", null)
      assert(uriExchange(testURIExchange04) == "mult")
    }
  }

  "uriRoutingKey" should {
    "return the routing key of an instance of URI" in {
      val testURI02: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=quux", null)
      assert(uriRoutingKey(testURI02) == "quux")
    }
  }
}
