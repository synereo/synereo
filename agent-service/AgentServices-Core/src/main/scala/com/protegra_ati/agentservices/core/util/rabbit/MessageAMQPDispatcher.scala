package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import _root_.scala.actors.Actor
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import _root_.java.util.Timer
import _root_.java.util.TimerTask
import com.biosimilarity.lift.lib.amqp.RabbitFactory


class MessageAMQPDispatcher(factory: ConnectionFactory, exchange: String, routingKey: String)
    extends AMQPDispatcher[Message](RabbitFactory.getConnection(factory, factory.getHost, factory.getPort)) {

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

class MessageAMQPListener(config: RabbitConfiguration, exchange: String, routingKey: String, handleMessage: (Message) => Unit) {
  val factory = RabbitFactory.guest
  factory.setPassword(config.password)
  factory.setUsername(config.userId)
  factory.setHost(config.host)
  factory.setPort(config.port)

  val amqp = new MessageAMQPDispatcher(factory, exchange, routingKey)
  amqp.start

  class MessageListener(handler: (Message) => Unit) extends Actor {
    def act = {
      react {
//	case msg@AMQPMessage(contents: Message) => println("exchange: " + exchange + " for message id: " + contents.ids.id); handler(contents); act
        case msg@AMQPMessage(contents: Message) => handler(contents); act
      }
    }
  }

  val messageListener = new MessageListener(handleMessage(_: Message))
  messageListener.start
  amqp ! AMQPAddListener(messageListener)
}


