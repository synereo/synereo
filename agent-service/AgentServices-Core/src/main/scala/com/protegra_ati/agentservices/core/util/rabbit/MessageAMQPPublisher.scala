package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import actors.Actor
import com.biosimilarity.lift.lib.amqp.RabbitFactory

class MessageAMQPSender(factory: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String)
  extends AMQPSender[ Message ](RabbitFactory.getConnection(factory, host, port), exchange, routingKey)
{
}

class MessageAMQPPublisher(host: String, port: Int, exchange: String, routingKey: String)
{
  //under the covers
  //  val queueName = exchangeName + "_queue"
  val amqp = new MessageAMQPSender(RabbitFactory.guest, host, port, exchange, routingKey)
  amqp.start

  def send(value: Message) =
  {
    amqp ! AMQPMessage(value)
  }
}
