package com.protegra_ati.agentservices.core.util.rabbit

import com.protegra_ati.agentservices.core.messages.Message
import java.io.{IOException, ByteArrayOutputStream, ObjectOutputStream}
import java.util.concurrent.{ThreadFactory, TimeUnit, Executors}
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}
import java.util.concurrent.atomic.AtomicInteger
import org.joda.time.DateTime
import java.security.MessageDigest
import com.protegra_ati.agentservices.core.util.serializer.Serializer

object AMQPPublisherThreadFactory extends ThreadFactory
{
  final val threadNumber = new AtomicInteger(0)
  final val poolName = DateTime.now.toString("HHmmss")

  def newThread(r: Runnable) = {
    val t = Executors.defaultThreadFactory().newThread(r)
    t.setName("AMQPPublisher-" + poolName + "-thread-" + threadNumber.incrementAndGet() )
    t
  }
}

object MessageAMQPPublisher extends Reporting
{
  // Used for sendToRabbit retries
  @transient
  private lazy val scheduler = Executors.newScheduledThreadPool(5, AMQPPublisherThreadFactory)

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

  def sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message, delay: Long): Unit =
  {
    val success = _sendToRabbit(config, exchange, routingKey, message)

    if (!success) {
      report("Message not sent to rabbit, rescheduling, delay: " + delay, Severity.Info)
      scheduler.schedule(new Runnable() {
        def run(): Unit = {
          val newDelay = (delay*2).max(MIN_RETRY).min(MAX_RETRY) // Retry 1s to 10min
          sendToRabbit(config, exchange, routingKey, message, newDelay)
        }
      }, delay, TimeUnit.MILLISECONDS)
    }
  }

  private def _sendToRabbit(config: RabbitConfiguration, exchange: String, routingKey: String, message: Message): Boolean = {
    val factory = RabbitMQFactory.guest
    factory.setPassword(config.password)
    factory.setUsername(config.userId)
    factory.setHost(config.host)
    factory.setPort(config.port)

    var success:Boolean = false

    try {
      val conn = RabbitMQFactory.getConnection(factory, config.host, config.port)
      val channel = conn.createChannel()
      try {
        // Now write an object to a byte array and shove it across the wire.
        val bytes = Serializer.serializeToBytes(message)

        val qname = ( exchange + "_queue" )
        channel.exchangeDeclare(exchange, "direct")
        channel.queueDeclare(qname, true, false, false, null)
        channel.queueBind(qname, exchange, routingKey)
        channel.basicPublish(exchange, routingKey, null, bytes)

        success = true
      }
      finally {
        // Close channel when done, ignoring exceptions
        try { channel.close() } catch { case e: Exception => }
      }
    } catch {
      case e: Exception => {
        val m = "There was a problem sending message to Rabbit.  Exchange: %s  routingKey: %s  message: %s"
        val fm = m.format(exchange, routingKey, message)
        report(fm, e, Severity.Error)
      }
    }

    success
  }
}