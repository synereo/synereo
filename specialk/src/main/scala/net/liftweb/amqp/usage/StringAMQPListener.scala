package net.liftweb.amqp.usage

import actors.Actor
import net.liftweb.amqp._
import com.rabbitmq.client._
import com.biosimilarity.lift.lib.amqp.RabbitFactory


//receiving
class StringAMQPDispatcher(factory: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String)
  extends AMQPDispatcher[ String ](RabbitFactory.getConnection(factory, host, port))
{

  override def configure(channel: Channel)
  {
    // Set up the exchange and queue
    val queueName = exchange + "_queue"

    channel.exchangeDeclare(exchange, "direct")
    channel.queueDeclare(queueName, true, false, false, null);
    channel.queueBind(queueName, exchange, routingKey)
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume(queueName, false, new SerializedConsumer(channel, this))
  }
}

class StringAMQPListener(host: String, port: Int, exchange: String, routingKey: String, handleString: ( String ) => Unit)
{
  val amqp = new StringAMQPDispatcher(RabbitFactory.guest, host, port, exchange, routingKey)
  amqp.start

  class StringListener(handler: ( String ) => Unit) extends Actor
  {
    def act =
    {
      react {
//        case msg@AMQPMessage(contents: String) => println("exchange: " + exchange + " for value: " + contents); handler(contents); act
        case msg@AMQPMessage(contents: String) => handler(contents); act
      }
    }
  }

  val messageListener = new StringListener(handleString(_: String))
  messageListener.start
  amqp ! AMQPAddListener(messageListener)
}
