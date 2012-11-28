package net.liftweb.amqp

import _root_.scala.actors.Actor
import _root_.com.rabbitmq.client._
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ObjectOutputStream
import com.biosimilarity.lift.lib.amqp.RabbitFactory

//uses 1 static connection for all on a host:port
abstract class AMQPSender[ T ](conn: Connection, exchange: String, routingKey: String) extends Actor
{
  val channel = conn.createChannel()

  def send(msg: T)
  {
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
  }

  def act = loop

  def loop
  {
    react {
      case AMQPMessage(msg: T) => send(msg); loop
    }
  }
}

/**
 * An actor with a long-lived connection to an AMQP exchange/queue.
 *
 * @see ExampleStringAMQPSender for an example use.
 * @author Steve Jenson (stevej@pobox.com)
 */
//abstract class AMQPSender[T](cf: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String) extends Actor {
//  val conn = cf.newConnection( Array { new Address(host, port) } )
//  val channel = conn.createChannel()
//
//  /**
//   * Override this to use your own AMQP queue/exchange with the given channel.
//  */
//  def configure(channel: Channel)
//
//  def send(msg: T) {
//    // Now write an object to a byte array and shove it across the wire.
//    val bytes = new ByteArrayOutputStream
//    val store = new ObjectOutputStream(bytes)
//    store.writeObject(msg)
//    store.close
//
//    val qname = (exchange + "_queue")
//    channel.exchangeDeclare( exchange, "direct" )
//    //queueDeclare(java.lang.String queue, boolean durable, boolean exclusive, boolean autoDelete, java.util.Map<java.lang.String,java.lang.Object> arguments)
//    channel.queueDeclare(qname, true, false, false, null);
//    channel.queueBind( qname, exchange, routingKey )
//
//    channel.basicPublish(exchange, routingKey, null, bytes.toByteArray)
//  }
//
//  def act = loop
//
//  def loop {
//    react {
//      case AMQPMessage(msg: T) => send(msg); loop
//    }
//  }
//}

/**
 * An example subclass of AMQPSender[T]
 *
 * An example of how to send messages to an AMQP queue/exchange. Notice that this
 * is setup with the same params as StringAQMPExample. After making a new instance of
 * StringAMQPExample, just send ExampleAMQPSender ! "hi" to see the message "hi"
 * appear in the output log. Fun and Easy!
 *
 * If you are planning to send lots of messages to lots of different exchange/queues,
 * consider creating Actor-based Senders, that will help your application to scale.
 */
class StringAMQPSender(cf: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String) extends AMQPSender[String](RabbitFactory.getConnection(cf, host, port), exchange, routingKey) {

}

/**
 * An Example of how to use the Example subclass of AMQPSender[T]. Still following?
 */
class ExampleStringAMQPSender {
  // All of the params, exchanges, and queues are all just example data.
  val factory = new ConnectionFactory()
  factory.setUsername("guest")
  factory.setPassword("guest")
  factory.setVirtualHost("/")
  factory.setRequestedHeartbeat(0)

  val amqp = new StringAMQPSender(factory, "localhost", 5672, "mult", "routeroute")
  amqp.start
  amqp ! AMQPMessage("hi")
}

/**
 * An example of using AMQP in a short-lived manner, setting up and tearing down
 * the connection whenever you need it. The long-lived example above is more
 * efficient with resources.
 */
object ExampleDirectAMQPSender {
  def send[T](msg: T) {
    // All of the params, exchanges, and queues are all just example data.
    val factory = new ConnectionFactory()
    factory.setUsername("guest")
    factory.setPassword("guest")
    factory.setVirtualHost("/")
    factory.setRequestedHeartbeat(0)
    send(msg, factory, "localhost", 5672)
  }

  def send[T](msg: T, factory: ConnectionFactory, host: String, port: Int) {
    val conn = RabbitFactory.getConnection(factory, host, port)
    val channel = conn.createChannel()
    // Now write an object to a byte array and shove it across the wire.
    val bytes = new ByteArrayOutputStream
    val store = new ObjectOutputStream(bytes)
    store.writeObject(msg)
    store.close
    channel.basicPublish("mult", "routeroute", null, bytes.toByteArray)
  }
}

/**
 * Just a few examples of how you might test this. Mostly useful snippets
 * for me to run from script/console to see the bits flying.
 */
object AMQPExampleFunPack {
  def directExample {
    val recv = new ExampleStringAMQPListener()
    // You should see the message 'hi'
    val sender = new ExampleStringAMQPSender()
    sender
  }
  def actorExample {
    val recv = new ExampleStringAMQPListener()
    // You probably know what message you are going to see. 'hello!'
    val sender = ExampleDirectAMQPSender.send("hello!")
    sender
  }
}
