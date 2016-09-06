package com.biosimilarity.lift.lib.amqp

import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}

import scala.util.Try

object AMQPUtil {

  def rabbitIsRunning(host: String = "localhost", port: Int = 5672): Boolean =
    Try {
      val factory: ConnectionFactory = new ConnectionFactory
      factory.setHost(host)
      factory.setPort(port)
      val conn: Connection           = factory.newConnection
      val channel: Channel           = conn.createChannel
      channel.close()
      conn.close()
    }.isSuccess
}
