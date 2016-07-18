package com.biosimilarity.lift.lib

import org.scalatest.{Matchers, WordSpec}

class AMQPMonikerOpsSpec extends WordSpec with Matchers {

  import java.net.URI

  import com.biosimilarity.lift.lib.moniker.Moniker

  // Implicit coversions from URI to Moniker
  import com.biosimilarity.lift.lib.moniker.identityConversions._

  // AMQPMonikerOps is a trait
  object AMQPMonikerOpsInstance extends AMQPMonikerOps
  import AMQPMonikerOpsInstance._

  val testURI: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=routeroute", null)

  "an instance of java.net.URI" should {
    "be implicitly converted to a Moniker" in {
      (testURI: Moniker) shouldBe a[Moniker]
    }
  }

  "mnkrHost" should {
    "return the host of an instance of URI" in {
      assert(mnkrHost(testURI) == "localhost")
    }
  }

  "mnkrPort" should {
    "return the port of an instance of URI" in {
      assert(mnkrPort(testURI) == 5672)
    }
  }

  "mnkrExchange" should {

    "return the default exchange for an instance of URI containing no exchange" in {
      val testURINoExchange: URI = new URI("amqp", null, "localhost", 5672, null, null, null)
      assert(mnkrExchange(testURINoExchange) == "mult")
    }

    "return the exchange of an instance of URI" in {
      assert(mnkrExchange(testURI) == "mult")
    }

    "return the exchange of an instance of URI (2)" in {
      val testURIExchange02: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult", "routingKey=routeroute", null)
      assert(mnkrExchange(testURIExchange02) == "mult")
    }

    "return the exchange of an instance of URI (3)" in {
      val testURIExchange03: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult/iult", "routingKey=routeroute", null)
      assert(mnkrExchange(testURIExchange03) == "mult")
    }

    "return the exchange of an instance of URI (4)" in {
      val testURIExchange04: URI = new URI("amqp", null, "localhost", 5672, "/mult/kult/iult/fglt", "routingKey=routeroute", null)
      assert(mnkrExchange(testURIExchange04) == "mult")
    }
  }

  "mnkrRoutingKey" should {

    "return the default routing key for an instance of URI containing no routing key" in {
      val testURINoRoutingKey: URI = new URI("amqp", null, "localhost", 5672, "/mult", null, null)
      assert(mnkrRoutingKey(testURINoRoutingKey) == "routeroute")
    }

    "return the routing key of an instance of URI" in {
      val testURI02: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=quux", null)
      assert(mnkrRoutingKey(testURI02) == "quux")
    }
  }
}
