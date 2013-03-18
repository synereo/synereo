package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import scala.actors.Actor
import com.biosimilarity.lift.lib.amqp.RabbitFactory

class MessageAMQPSender(factory: ConnectionFactory, exchange: String, routingKey: String) extends Actor
  //extends AMQPSender[ Message ](, exchange, routingKey)
{
  def send(msg: Message)
  {
    val conn = RabbitFactory.getConnection(factory, factory.getHost, factory.getPort)
    val channel = conn.createChannel()
    try {
      // Now write an object to a byte array and shove it across the wire.
      val bytes = new ByteArrayOutputStream
      val store = new ObjectOutputStream(bytes)
      store.writeObject(msg)
      store.close

      val qname = ( exchange + "_queue" )
      channel.exchangeDeclare(exchange, "direct")
      //queueDeclare(java.lang.String queue, boolean durable, boolean exclusive, boolean autoDelete, java.util.Map<java.lang.String,java.lang.Object> arguments)
      channel.queueDeclare(qname, true, false, false, null);
      channel.queueBind(qname, exchange, routingKey)

      channel.basicPublish(exchange, routingKey, null, bytes.toByteArray)
    } finally {
      channel.close
    }
  }

  def act = loop

  def loop
  {
    react {
      case AMQPMessage(msg: Message) => send(msg); loop
    }
  }
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
