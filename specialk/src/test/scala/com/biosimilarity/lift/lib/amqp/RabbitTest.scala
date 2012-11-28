package com.biosimilarity.lift.lib.amqp {

import usage._

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import java.util.UUID

class RabbitTest
  extends JUnit4(RabbitTestSpecs)

object RabbitTestSpecsRunner
  extends ConsoleRunner(RabbitTestSpecs)

object RabbitTestSpecs extends Specification
{
  "basicPublish" should {
    "be found by basicConsume" in {

      def handleString(msg: String) =
      {
        println("received: " + msg)
      }

      val exchange = "mult"
      val exchangeContent = "123content(_)"
      val exchangeSearch = "search"
      val routingKey = "routeroute"

      val default = new StringAMQPListener("localhost", 5672, exchange, routingKey, handleString(_: String))
      val content = new StringAMQPListener("localhost", 5672, exchangeContent, routingKey, handleString(_: String))
      val search = new StringAMQPListener("localhost", 5672, exchangeSearch, routingKey, handleString(_: String))
      for ( i <- 1 to 10000 ) {
        val a = new StringAMQPPublisher("localhost", 5672, exchangeContent, routingKey)
        a.send("msg: " + i)
      }
      Thread.sleep(10000)
    }
  }

  "basicPublish" should {
    "be found by basicConsume" in {

      def handleString(msg: String) =
      {
        println("received: " + msg)
      }

      val exchange = "mult"
      val exchangeContent = "123content(_)"
      val exchangeSearch = "search"
      val exchangeRandom = "random"
      val routingKey = "routeroute"

      val default = new StringAMQPListener("localhost", 5672, exchange, routingKey, handleString(_: String))
      val content = new StringAMQPListener("localhost", 5672, exchangeContent, routingKey, handleString(_: String))
      val search = new StringAMQPListener("localhost", 5672, exchangeSearch, routingKey, handleString(_: String))
      for ( i <- 1 to 1000 ) {
        val a = new StringAMQPPublisher("localhost", 5672, exchangeContent, routingKey)
        a.send("msg: " + i)
      }
      for ( i <- 1 to 1000 ) {
        val b = new StringAMQPPublisher("localhost", 5672, exchangeSearch, routingKey)
        b.send("msg: " + i)
      }

      //no consumer on this one - uncomment if you want to see that it doesn't mess up the other exchanges
//      for ( i <- 1 to 1000 ) {
//        val c = new StringAMQPPublisher("localhost", 5672, exchangeRandom + i, routingKey)
//        c.send("msg: " + i)
//      }
      Thread.sleep(5000)
    }
  }
}

}
