package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import _root_.scala.actors.Actor
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}

class MessageAMQPListener(host: String, port: Int, exchange: String, routingKey: String, handleMessage: (Message) => Unit) {
  val amqp = new MessageAMQPDispatcher(RabbitFactory.guest, host, port, exchange, routingKey)
  amqp.start

  class MessageListener(handler: (Message) => Unit) extends Actor {
    def act = {
      react {
	case msg@AMQPMessage(contents: Message) => println("exchange: " + exchange + " for message id: " + contents.ids.id); handler(contents); act
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

