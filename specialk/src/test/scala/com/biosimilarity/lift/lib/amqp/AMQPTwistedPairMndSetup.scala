package com.biosimilarity.lift.lib

import java.net.URI

import com.rabbitmq.client.AMQP.{Exchange, Queue}
import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}

import scala.collection.mutable

object AMQPTwistedPairMndSetup {

  def roundTrip[A](src: URI, trgt: URI, qName: String, produced: Set[A], consumed: mutable.Set[A]): () => Unit = {
    val scope: AMQPTwistedPairScope[A] with AMQPBrokerScope[A] with MonadicDispatcherScope[A] = AMQPStdTPS[A](src, trgt)
    val qpM: scope.AMQPQueueMQT[A, scope.AMQPAbstractQueue] =
      new scope.TwistedQueuePairM[A](qName, "routeroute").asInstanceOf[scope.AMQPQueueMQT[A, scope.AMQPAbstractQueue]]
    val qtp: scope.AMQPAbstractQueue[A] = qpM.zero[A]

    val cleanup: () => Unit = { () =>
      val factory: ConnectionFactory          = scope.factory
      val conn: Connection                    = factory.newConnection()
      val channel: Channel                    = conn.createChannel()
      val exchangeDeleteOk: Exchange.DeleteOk = channel.exchangeDelete(qName)
      val queueDeleteOk: Queue.DeleteOk       = channel.queueDelete(qName + "_queue")
      channel.close()
      conn.close()
    }

    qpM(qtp).foreach { (msg: A) =>
      consumed += msg
    }

    produced.foreach { (v: A) =>
      qtp ! v
    }

    cleanup
  }

  val localhost: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=routeroute", null)
}
