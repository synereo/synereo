package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import _root_.scala.actors.Actor
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}

class MessageAMQPListener(host: String, port: Int, exchange: String, routingKey: String, handleMessage: (Message) => Unit) {
  val factory = new ConnectionFactory()
  factory.setUsername("guest")
  factory.setPassword("guest")
  factory.setVirtualHost("/")
  factory.setRequestedHeartbeat(0)


  val amqp = new MessageAMQPDispatcher(factory, host, port, exchange, routingKey)
  amqp.start

  class MessageListener(handler: (Message) => Unit) extends Actor {
    def act = {
      react {
	case msg@AMQPMessage(contents: Message) => System.err.println("exchange: " + exchange); handler(contents); act
      }
    }
  }

  val messageListener = new MessageListener(handleMessage(_: Message))
  messageListener.start
  amqp ! AMQPAddListener(messageListener)
}

class MessageAMQPDispatcher(factory: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String)
    extends AMQPDispatcher[Message](factory, host, port) {
  override def configure(channel: Channel) {
    // Set up the exchange and queue
    val queueName = exchange + "_queue"

    channel.exchangeDeclare(exchange, "direct")
    channel.queueDeclare(queueName, true, false, false, null);
    channel.queueBind(queueName, exchange, routingKey)
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume(queueName, false, new SerializedConsumer(channel, this))
  }
}

