package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}

class MessageAMQPSender(cf: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String) extends AMQPSender[Message](cf, host, port, exchange, routingKey) {
  override def configure(channel: Channel) = {
    //BUGBUG: JSK - is this internal code needed anymore now that ticket is obsolete
    val conn = cf.newConnection( Array { new Address(host, port) } )
    val channel = conn.createChannel()
  }
}

class MessageAMQPPublisher(host: String, port: Int, exchange: String, routingKey: String) {
  // All of the params, exchanges, and queues are all just example data.
  val factory = new ConnectionFactory()
  factory.setUsername("guest")
  factory.setPassword("guest")
  factory.setVirtualHost("/")
  factory.setRequestedHeartbeat(0)

  //under the covers
//  val queueName = exchangeName + "_queue"
  val amqp = new MessageAMQPSender(factory, host, port, exchange, routingKey)
  amqp.start

  def send(value: Message) = {
    amqp ! AMQPMessage(value)
  }
}