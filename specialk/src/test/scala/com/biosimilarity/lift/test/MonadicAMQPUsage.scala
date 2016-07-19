package com.biosimilarity.lift.test

import com.biosimilarity.lift.lib.{AMQPDefaults, SMJATwistedPair}

/**
  * lifted from MonadicAMQP.scala
  */
class MonadicAMQPUsage extends AMQPTestUtility[String] {

  import java.net.URI

  import AMQPDefaults._
  import com.biosimilarity.lift.lib.moniker.identityConversions._

  override def msgStreamPayload(idx: Int): String = { "Msg" + idx }

  val localhost: URI = new URI("amqp", null, "localhost", 5672, "/mult", "routingKey=routeroute", null)

  trait Destination
  case object Src  extends Destination
  case object Trgt extends Destination

  def smjatp(d: Destination): SMJATwistedPair[Message] = {
    val _smjatp = d match {
      case Src  => SMJATwistedPair[Message](localhost, localhost)
      case Trgt => SMJATwistedPair[Message](localhost, localhost)
    }
    _smjatp.jsonDispatcher { (msg: Message) =>
      println("received : " + msg)
    }
    val msgs = msgStream.take(100).toList
    _smjatp.jsonSender
    // Msg has been changed to Message to get this to compile, but...
    for (i <- 1 to 100) {
      _smjatp.send(msgs(i - 1))
    }
    _smjatp
  }
}
