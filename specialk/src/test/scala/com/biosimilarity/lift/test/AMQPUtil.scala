package com.biosimilarity.lift.test

import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}

object AMQPUtil {

  def rabbitIsRunning(): Boolean =
    try {
      val factory: ConnectionFactory = new ConnectionFactory
      val conn: Connection           = factory.newConnection
      val channel: Channel           = conn.createChannel
      channel.close()
      conn.close()
      true
    } catch {
      case e: Throwable => false
    }

  def genRandQueueName(): String = s"test_${java.util.UUID.randomUUID()}"
}
