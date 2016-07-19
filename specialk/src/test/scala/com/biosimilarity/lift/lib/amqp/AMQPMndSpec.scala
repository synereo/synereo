package com.biosimilarity.lift.lib

import com.biosimilarity.lift.test.AMQPUtil
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{Matchers, WordSpec}

class AMQPMndSpec extends WordSpec with Matchers with Eventually with IntegrationPatience {

  import com.biosimilarity.lift.lib.AMQPMndSetup._

  import scala.collection.mutable

  assume(AMQPUtil.rabbitIsRunning(), "Could not connect to RabbitMQ")

  "A set of integers received over AMQP" should {
    "contain the same elements as the set of integers which are eventually received" in {
      val sendSet: Set[Int]            = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val receiveSet: mutable.Set[Int] = mutable.Set.empty[Int]
      val cleanup: () => Unit          = roundTrip[Int]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }

    "contain the same elements as the set of integers which are eventually received (2) " in {
      val sendSet: Set[Int]            = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val receiveSet: mutable.Set[Int] = mutable.Set.empty[Int]
      val cleanup: () => Unit          = roundTrip[Int]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }
  }

  "An empty set of integers (not) sent over AMQP" should {
    "contain the same elements as the set of integers which are eventually (not) received" in {
      val sendSet: Set[Int]            = Set.empty[Int]
      val receiveSet: mutable.Set[Int] = mutable.Set.empty[Int]
      val cleanup: () => Unit          = roundTrip[Int]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }
  }

  "A set of doubles sent over AMQP" should {
    "contain the same elements as the set of doubles which are eventually received" in {
      val sendSet: Set[Double]            = Set(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
      val receiveSet: mutable.Set[Double] = mutable.Set.empty[Double]
      val cleanup: () => Unit             = roundTrip[Double]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }

    "contain the same elements as the set of doubles which are eventually received (2) " in {
      val sendSet: Set[Double]            = Set(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
      val receiveSet: mutable.Set[Double] = mutable.Set.empty[Double]
      val cleanup: () => Unit             = roundTrip[Double]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }
  }

  "A set of strings sent over AMQP" should {
    "contain the same elements as the set of strings which are eventually received" in {
      val sendSet: Set[String] = Set("In Xanadu did Kubla Khan",
                                     "A stately pleasure-dome decree:",
                                     "Where Alph, the sacred river, ran",
                                     "Through caverns measureless to man",
                                     "Down to a sunless sea.")
      val receiveSet: mutable.Set[String] = mutable.Set.empty[String]
      val cleanup: () => Unit             = roundTrip[String]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }
  }

  "An empty set of strings (not) sent over AMQP" should {
    "contain the same elements as the set of strings which are eventually (not) received" in {
      val sendSet: Set[String]            = Set.empty[String]
      val receiveSet: mutable.Set[String] = mutable.Set.empty[String]
      val cleanup: () => Unit             = roundTrip[String]("localhost", "localhost", AMQPUtil.genRandQueueName(), sendSet, receiveSet)
      eventually { receiveSet should contain theSameElementsAs sendSet }
      cleanup()
    }
  }
}
