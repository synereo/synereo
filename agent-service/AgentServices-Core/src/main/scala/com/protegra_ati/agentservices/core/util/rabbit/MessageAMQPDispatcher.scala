package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import _root_.scala.actors.Actor
import com.protegra_ati.agentservices.core.messages.{EventKey, Message}
import _root_.java.util.Timer
import _root_.java.util.TimerTask
import com.biosimilarity.lift.lib.amqp.RabbitFactory


class MessageAMQPDispatcher(config: RabbitConfiguration, exchange: String, routingKey: String) extends Actor {
  var connection = createConnection()
  var channel = createChannel()

  def getFactory() = {
    val factory = RabbitMQFactory.guest
    factory.setRequestedHeartbeat(5)
    factory.setPassword(config.password)
    factory.setUsername(config.userId)
    factory.setHost(config.host)
    factory.setPort(config.port)

    factory
  }

  def createConnection(): Connection = {
    val conn = RabbitMQFactory.getConnection(getFactory, getFactory.getHost, getFactory.getPort)
    conn
  }

  def createChannel(): Channel = {
    val amqp = this
    connection.addShutdownListener(new ShutdownListener {
      def shutdownCompleted(cause: ShutdownSignalException) {
        println("CONNECTION Shutdown Detected, will try to reconnect...")
        amqp ! AMQPReconnect(1000)
      }
    })

    val channel = connection.createChannel()
    configure(channel)
    channel
  }

  def configure(channel: Channel) {
    // Set up the exchange and queue
    val queueName = exchange + "_queue"

    channel.exchangeDeclare(exchange, "direct")
    channel.queueDeclare(queueName, true, false, false, null);
    channel.queueBind(queueName, exchange, routingKey)
    channel.basicConsume(queueName, false, new SerializedConsumer(channel, this))
  }

  def act = loop(Nil)

  private lazy val reconnectTimer = new Timer("AMQPReconnectTimer")

  def loop(as: List[Actor]) {
    react {
      case AMQPAddListener(a) => loop(a :: as)
      case msg@AMQPMessage(t) => as.foreach(_ ! msg); loop(as)
      case AMQPReconnect(delay: Long) => {
        try {
          connection = createConnection()
          channel = createChannel()
          println("AMQPDispatcher: Successfully reconnected to AMQP Server")
        } catch {
          // Attempts to reconnect again using geometric back-off.
          case e: Exception => {
            val amqp = this
            println("AMQPDispatcher: Will attempt reconnect again in " + (delay * 2) + "ms.")
            reconnectTimer.schedule(new TimerTask() {
              override def run = {
                amqp ! AMQPReconnect(delay * 2)
              }}, delay)
          }
        }
        loop(as)
      }
      case _ => loop(as)
    }
  }
}

class MessageAMQPListener(config: RabbitConfiguration, exchange: String, routingKey: String, handleMessage: (Message) => Unit) {
  val amqp = new MessageAMQPDispatcher(config, exchange, routingKey)
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


