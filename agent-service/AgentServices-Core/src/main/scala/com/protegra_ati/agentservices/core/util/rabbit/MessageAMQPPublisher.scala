package com.protegra_ati.agentservices.core.util.rabbit

import net.liftweb.amqp._
import com.rabbitmq.client._
import com.protegra_ati.agentservices.core.messages.Message
import java.io.{IOException, ByteArrayOutputStream, ObjectOutputStream}
import com.biosimilarity.lift.lib.amqp.RabbitFactory
import java.util.concurrent.{TimeUnit, Executors}
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

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

object MessageAMQPPublisher extends Reporting
{
  // Used for sendToRabbit retries
  @transient
  private lazy val scheduler = Executors.newScheduledThreadPool(5)

  private val MIN_RETRY = 1000
  private val MAX_RETRY = 300000

  /**
   * Sends a message synchronously to Rabbit, creating a new Channel and closing it once finished.
   * @param config
   * @param exchange
   * @param routingKey
   * @param message
   */
  def sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message): Unit =
  {
    sendToRabbit(config, exchange, routingKey, message, 0)
  }

  def sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message, delay: Int): Unit =
  {
    val success = _sendToRabbit(config, exchange, routingKey, message)

    if (!success) {
      scheduler.schedule(new Runnable() {
        def run(): Unit = {
          val newDelay = (delay*2).max(MIN_RETRY).min(MAX_RETRY) // Retry 1s to 10min
          sendToRabbit(config, exchange, routingKey, message, newDelay)
        }
      }, delay, TimeUnit.MILLISECONDS)
    }
  }

  private def _sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message): Boolean = {
    val factory = RabbitFactory.guest
    factory.setPassword(config.password)
    factory.setUsername(config.userId)
    factory.setHost(config.host)
    factory.setPort(config.port)

    var success:Boolean = false

    try {
      val conn = factory.newConnection(Array {new Address(config.host, config.port)})
      try {
        val channel = conn.createChannel()
        // Now write an object to a byte array and shove it across the wire.
        val bytes = new ByteArrayOutputStream
        val store = new ObjectOutputStream(bytes)
        store.writeObject(message)
        store.close

        val qname = ( exchange + "_queue" )
        channel.exchangeDeclare(exchange, "direct")
        channel.queueDeclare(qname, true, false, false, null);
        channel.queueBind(qname, exchange, routingKey)
        channel.basicPublish(exchange, routingKey, null, bytes.toByteArray)

        success = true
      }
      finally {
        // Close connection (auto-closes channel) when done
        try { conn.close() } catch { case e => {} }
      }
    } catch {
      case e: IOException => {
        val m = "There was a problem sending message to Rabbit.  Exchange: %s  routingKey: %s  message: %s"
        val fm = m.format(exchange, routingKey, message)
        report(fm, e, Severity.Error)
      }
    }

    success
  }
}