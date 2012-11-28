package net.liftweb.amqp.usage

import actors.Actor
import net.liftweb.amqp._
import com.rabbitmq.client._
import com.biosimilarity.lift.lib.amqp.RabbitFactory

//sending
class StringAMQPSender(factory: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String)
  extends AMQPSender[ String ](RabbitFactory.getConnection(factory, host, port), exchange, routingKey)
{
}

class StringAMQPPublisher(host: String, port: Int, exchange: String, routingKey: String)
{
  //under the covers
  //  val queueName = exchangeName + "_queue"
  val amqp = new StringAMQPSender(RabbitFactory.guest, host, port, exchange, routingKey)
  amqp.start

  def send(value: String) =
  {
    amqp ! AMQPMessage(value)
  }
}
