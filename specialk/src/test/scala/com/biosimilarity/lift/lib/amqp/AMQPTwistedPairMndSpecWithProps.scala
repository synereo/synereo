package com.biosimilarity.lift.lib.amqp

import com.biosimilarity.lift.test.AMQPUtil
import com.biosimilarity.lift.test.Generators._
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class AMQPTwistedPairMndSpecWithProps
    extends WordSpec
    with Matchers
    with Eventually
    with IntegrationPatience
    with GeneratorDrivenPropertyChecks {

  import com.biosimilarity.lift.lib.AMQPTwistedPairMndSetup._

  import scala.collection.mutable

  // ht 2016.07.16: We should increase minSuccessful, but you gotta start somewhere
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 1, minSize = 100, maxSize = 100)

  assume(AMQPUtil.rabbitIsRunning(), "Could not connect to RabbitMQ")

  "A random set of strings sent over AMQP" should {
    "contain the same elements as the set of strings which are eventually received" in {
      forAll(nonEmptySetOfNonEmptyStrings) { (sendSet: Set[String]) =>
        whenever(sendSet.forall(_.nonEmpty)) {
          val receiveSet: mutable.Set[String] = mutable.Set.empty[String]
          val cleanup: () => Unit             = roundTrip[String](localhost, localhost, AMQPUtil.genRandQueueName(), sendSet, receiveSet)
          eventually { receiveSet should contain theSameElementsAs sendSet }
          cleanup()
        }
      }
    }
  }
}
