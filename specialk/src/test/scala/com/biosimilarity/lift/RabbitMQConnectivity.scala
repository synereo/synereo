// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicTermStoreTests.scala 
// Authors:     lgm                                                    
// Creation:    Tue Apr  5 20:08:45 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.test.lib

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store.xml.datasets._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.model.store.usage._
import com.biosimilarity.lift.lib._
import moniker._
import com.biosimilarity.lift.lib.extensions.StringExtensions._
import com.biosimilarity.lift.lib.extensions.URIExtensions._
import com.biosimilarity.lift.lib.extensions.URMExtensions._
import com.biosimilarity.lift.lib.usage._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._

import org.prolog4j._

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.util.UUID


class RabbitMQConnectivityTest
extends JUnit4(RabbitMQConnectivityTestSpecs)

object RabbitMQConnectivityTestSpecsRunner
extends ConsoleRunner(RabbitMQConnectivityTestSpecs)


object RabbitMQConnectivityTestSpecs extends Specification
{

  val RABBIT_PORT_STANDARD = 5672
  val RABBIT_PORT_SOURCE = 5672
  val RABBIT_PORT_TARGET = 5672
// use these when wanting to test multi ports
//  val RABBIT_PORT_SOURCE = 4444
//  val RABBIT_PORT_TARGET = 5555
  import AMQPDefaults._

  case class Msg(
    s: String, i: Int, b: Boolean, r: Option[Msg]
  )

  "basic send / receive using monadic dispatching" should {

     "send and receive values over default rabbitmq queue" in {
      val sma1 = SMJATwistedPair[Msg]("localhost", "127.0.0.1")
      assertMonadicTwistedPair(sma1, sma1, "MonadicQueue1")
    }

    "send and receive values over standard rabbitmq queue" in {
      val sma1 = SMJATwistedPair[Msg]("localhost".toURM.withPort(RABBIT_PORT_STANDARD), ("127.0.0.1".toURM.withPort(RABBIT_PORT_STANDARD)))
      assertMonadicTwistedPair(sma1, sma1, "MonadicQueue2")
    }

    "send and receive values over custom port rabbitmq queue" in {
        val sma1 = SMJATwistedPair[Msg]("localhost".toURM.withPort(RABBIT_PORT_SOURCE), "127.0.0.1".toURM.withPort(RABBIT_PORT_SOURCE))
        assertMonadicTwistedPair(sma1, sma1, "MonadicQueue3")
      }

     "send and receive values over two separate rabbitmq queues" in {
      val writer = SMJATwistedPair[Msg]("localhost".toURM.withPort(RABBIT_PORT_SOURCE), "127.0.0.1".toURM.withPort(RABBIT_PORT_TARGET))
      val reader = SMJATwistedPair[Msg]("127.0.0.1".toURM.withPort(RABBIT_PORT_TARGET), "localhost".toURM.withPort(RABBIT_PORT_SOURCE))
      assertMonadicTwistedPair(writer, reader, "MonadicQueue4")
    }
  }

  def assertMonadicTwistedPair(writer: SMJATwistedPair[Msg], reader: SMJATwistedPair[Msg], exchangeName: String) = {
    var msgCount = 0
    var sleepCount = 0

    lazy val msgStrm = createMessageStream

    writer.jsonSender(exchangeName)
    reader.jsonDispatcher(
      exchangeName,
      (x) => {
        println("received : " + x)
        msgCount += 1
      }
    )
    val msgs = msgStrm.take(100)

    for (i <- 0 to 9) {writer.send(msgs(i))}

    while (msgCount < 10 && sleepCount < 25) {
      Thread.sleep(100)
      sleepCount += 1
      println("waiting for message " + msgCount + " attempt " + sleepCount)
    }

    msgCount must be >= 10
    sleepCount must be < 25
  }

   def createMessageStream: Stream[Msg] = {
      List(Msg(("msg" + 0), 0, true, None)).toStream append (
        createMessageStream.map(
        {
          (msg) => {
            msg match {
              case Msg(s, i, b, r) => {
                val j = i + 1
                Msg(
                  ("msg" + j), j, ((j % 2) == 0), Some(msg)
                )
              }
            }
          }
        }
        )
        )
   }

  "basic send / receive using agent monadic dispatching" should {
    def junction = MonadicMsgJunction
    var msgCount = 0
    var sleepCount = 0

    "send and receive values over standard rabbitmq queue" in {
       val sma1 = junction.SMAJATwistedPair("localhost", "127.0.0.1")

       sma1.jsonSender("AgentMonadicQueue1")
       sma1.jsonDispatcher(
         "AgentMonadicQueue1",
         (x) => {
           println("received : " + x)
           msgCount += 1
         }
       )

       //val msgs = msgStrm.take(100)
       //val msg = new mock.Msgs.DReq()
       for (i <- 0 to 10) {
         sma1.send(junction.MonadicDMsgs.protoDreq)
       }

       while (msgCount < 10 && sleepCount < 25) {
         Thread.sleep(100)
         sleepCount += 1
         println("waiting for message " + msgCount + " attempt " + sleepCount)
       }

       msgCount must be >= 10
       sleepCount must be < 25
     }

    "send and receive values over standard rabbitmq queue" in {
       val sma1 = junction.SMAJATwistedPair("localhost".toURM.withPort(RABBIT_PORT_STANDARD), "127.0.0.1".toURM.withPort(RABBIT_PORT_STANDARD))

       sma1.jsonSender("AgentMonadicQueue2")
       sma1.jsonDispatcher(
         "AgentMonadicQueue2",
         (x) => {
           println("received : " + x)
           msgCount += 1
         }
       )

       //val msgs = msgStrm.take(100)
       //val msg = new mock.Msgs.DReq()
       for (i <- 0 to 10) {
         sma1.send(junction.MonadicDMsgs.protoDreq)
       }

       while (msgCount < 10 && sleepCount < 25) {
         Thread.sleep(100)
         sleepCount += 1
         println("waiting for message " + msgCount + " attempt " + sleepCount)
       }

       msgCount must be >= 10
       sleepCount must be < 25
     }

    "send and receive values over custom rabbitmq queue" in {
//      val sma1 = junction.SMAJATwistedPair("localhost", "localhost")
      val sma1 = junction.SMAJATwistedPair("localhost".toURM.withPort(RABBIT_PORT_SOURCE), "127.0.0.1".toURM.withPort(RABBIT_PORT_SOURCE))

      sma1.jsonSender("AgentMonadicQueue3")
      sma1.jsonDispatcher(
        "AgentMonadicQueue3",
        (x) => {
          println("received : " + x)
          msgCount += 1
        }
      )

      //val msgs = msgStrm.take(100)
      //val msg = new mock.Msgs.DReq()
      for (i <- 0 to 10) {
        sma1.send(junction.MonadicDMsgs.protoDreq)
      }

      while (msgCount < 10 && sleepCount < 25) {
        Thread.sleep(100)
        sleepCount += 1
        println("waiting for message " + msgCount + " attempt " + sleepCount)
      }

      msgCount must be >= 10
      sleepCount must be < 25
    }

    "send and receive values over two separate rabbitmq queues" in {
//      val writer = junction.SMAJATwistedPair("localhost", "localhost")
//      val reader = junction.SMAJATwistedPair("localhost", "localhost")
      val writer = junction.SMAJATwistedPair("localhost".toURM.withPort(RABBIT_PORT_SOURCE), "127.0.0.1".toURM.withPort(RABBIT_PORT_TARGET))
      val reader = junction.SMAJATwistedPair("127.0.0.1".toURM.withPort(RABBIT_PORT_TARGET), "localhost".toURM.withPort(RABBIT_PORT_SOURCE))

      writer.jsonSender("AgentMonadicQueue4")
      reader.jsonDispatcher(
        "AgentMonadicQueue4",
        (x) => {
          println("received : " + x)
          msgCount += 1
        }
      )

      for (i <- 0 to 10) {
        writer.send(junction.MonadicDMsgs.protoDreq)
      }

      while (msgCount < 10 && sleepCount < 25) {
        Thread.sleep(100)
        sleepCount += 1
        println("waiting for message " + msgCount + " attempt " + sleepCount)
      }

      msgCount must be >= 10
      sleepCount must be < 25
    }
  }
}
