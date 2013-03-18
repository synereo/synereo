package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import scala.actors.Actor
import com.biosimilarity.lift.lib.amqp.RabbitFactory

class MessageAMQPSender(factory: ConnectionFactory, exchange: String, routingKey: String)
  extends AMQPSender[ Message ](RabbitFactory.getConnection(factory, factory.getHost, factory.getPort), exchange, routingKey)
{
}

/**
 * Note: Each time a new MessageAMQPPublisher is created, a new Rabbit channel is created
 * The channel is NEVER destroyed, unless channel.close is manually called from calling code.
 * If auto-close is intended, use the MessageAMQPPublisher.sendToRabbit companion code to
 * send a message to rabbit, creating a new channel and closing it when the message is sent.
 */
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

object MessageAMQPPublisher
{
  def sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message): Unit =
  {
    val factory = RabbitFactory.guest
    factory.setPassword(config.password)
    factory.setUsername(config.userId)
    factory.setHost(config.host)
    factory.setPort(config.port)

    val conn = RabbitFactory.getConnection(factory, factory.getHost, factory.getPort)
    val channel = conn.createChannel()
    try {
      // Now write an object to a byte array and shove it across the wire.
      val bytes = new ByteArrayOutputStream
      val store = new ObjectOutputStream(bytes)
      store.writeObject(message)
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
}