package com.biosimilarity.lift.lib.amqp

import java.util.UUID

import com.biosimilarity.lift.test.Generators._
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

/**
  * Based on examples from:
  * https://www.rabbitmq.com/api-guide.html
  */
class AMQPCounterExampleSpec extends WordSpec with Matchers with Eventually with IntegrationPatience with GeneratorDrivenPropertyChecks {

  import com.rabbitmq.client._

  import scala.collection.mutable

  // implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 1000, minSize = 10, maxSize = 1000, workers = 4)

  def withAMQPConsumer(factory: ConnectionFactory)(testFn: (Connection, String, String, mutable.Set[Array[Byte]]) => Unit): Unit = {
    val producerConn: Connection           = factory.newConnection
    val consumerConn: Connection           = factory.newConnection
    val consumerChannel: Channel           = consumerConn.createChannel()
    val consumerTag: String                = s"consTag_${UUID.randomUUID()}"
    val exchangeName: String               = s"exchange_${UUID.randomUUID()}"
    val qName: String                      = s"queue_${UUID.randomUUID()}"
    val routingKey: String                 = s"route_${UUID.randomUUID()}"
    val consumed: mutable.Set[Array[Byte]] = mutable.Set.empty[Array[Byte]]

    consumerChannel.exchangeDeclare(exchangeName, "direct", true)
    consumerChannel.queueDeclare(qName, false, false, false, null)
    consumerChannel.queueBind(qName, exchangeName, routingKey)

    val consumer: DefaultConsumer = new DefaultConsumer(consumerChannel) {
      override def handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: Array[Byte]): Unit = {
        // val routingKey: String  = envelope.getRoutingKey
        // val contentType: String = properties.getContentType
        val deliveryTag: Long = envelope.getDeliveryTag
        consumed += body
        consumerChannel.basicAck(deliveryTag, false)
      }
    }

    consumerChannel.basicConsume(qName, false, consumerTag, consumer)

    try
      testFn(producerConn, exchangeName, routingKey, consumed)
    finally {
      consumerChannel.basicCancel(consumerTag)
      consumerChannel.close()
      producerConn.close()
      consumerConn.close()
    }
  }

  val factory: ConnectionFactory = new ConnectionFactory

  "An AMQP DefaultConsumer" should {

    "receive all of the byte arrays it was sent by a AMQP Producer via basicPublish" in {
      withAMQPConsumer(factory) { (conn: Connection, exchangeName: String, routingKey: String, consumed: mutable.Set[Array[Byte]]) =>
        forAll(nonEmptyByteArray) { (produced: Array[Byte]) =>
          val producerChannel: Channel = conn.createChannel()
          producerChannel.basicPublish(exchangeName, routingKey, null, produced)
          eventually { consumed should contain(produced) }
          producerChannel.close()
        }
      }
    }

    "receive all of the byte arrays it was sent by a AMQP Producer via basicPublish (dup)" in {
      withAMQPConsumer(factory) { (conn: Connection, exchangeName: String, routingKey: String, consumed: mutable.Set[Array[Byte]]) =>
        forAll(nonEmptyByteArray) { (produced: Array[Byte]) =>
          val producerChannel: Channel = conn.createChannel()
          producerChannel.basicPublish(exchangeName, routingKey, null, produced)
          eventually { consumed should contain(produced) }
          producerChannel.close()
        }
      }
    }
  }
}
