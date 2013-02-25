package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import actors.Actor
import com.biosimilarity.lift.lib.amqp.RabbitFactory

class MessageAMQPSender(factory: ConnectionFactory, exchange: String, routingKey: String)
  extends AMQPSender[ Message ](RabbitFactory.getConnection(factory, factory.getHost, factory.getPort ), exchange, routingKey)
{
}

class MessageAMQPPublisher(config: RabbitConfiguration, exchange: String, routingKey: String)
{
  //under the covers
  //  val queueName = exchangeName + "_queue"
  val factory = RabbitFactory.guest
  factory.setPassword(config.password)
  factory.setUsername(config.userId)
  factory.setHost(config.host)
  factory.setPort(config.port)

  val amqp = new MessageAMQPSender(factory, exchange, routingKey)
  amqp.start

  def send(value: Message) =
  {
    amqp ! AMQPMessage(value)
  }
}
