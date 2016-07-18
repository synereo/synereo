package com.biosimilarity.lift.lib

import com.rabbitmq.client.AMQP.{Exchange, Queue}
import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}

import scala.collection.mutable

object AMQPMndSetup {

  // ht 2016.07.15
  // It would be better to use a fixture for setting up the AMQP connection,
  // but we'll need to wait until the AMQPMnd implementation doesn't rely on Path-dependent Types
  def roundTrip[A](src: String, trgt: String, qName: String, produced: Set[A], consumed: mutable.Set[A]): () => Unit = {
    val srcScope: AMQPStdScope[A]                   = new AMQPStdScope[A]()
    val trgtScope: AMQPStdScope[A]                  = new AMQPStdScope[A]()
    val srcQM: srcScope.AMQPQueueHostExchangeM[A]   = new srcScope.AMQPQueueHostExchangeM[A](src, qName)
    val trgtQM: trgtScope.AMQPQueueHostExchangeM[A] = new trgtScope.AMQPQueueHostExchangeM[A](trgt, qName)
    val srcQ: srcScope.AMQPQueue[A]                 = srcQM.zero[A]
    val trgtQ: trgtScope.AMQPQueue[A]               = trgtQM.zero[A]

    val cleanup: () => Unit = { () =>
      val factory: ConnectionFactory          = srcScope.factory
      val conn: Connection                    = factory.newConnection()
      val channel: Channel                    = conn.createChannel()
      val exchangeDeleteOk: Exchange.DeleteOk = channel.exchangeDelete(qName)
      val queueDeleteOk: Queue.DeleteOk       = channel.queueDelete(qName + "_queue")
      channel.close()
      conn.close()
    }

    trgtQM(trgtQ).foreach { (msg: A) =>
      consumed += msg
    }

    produced.foreach { (v: A) =>
      srcQ ! v
    }

    cleanup
  }
}
