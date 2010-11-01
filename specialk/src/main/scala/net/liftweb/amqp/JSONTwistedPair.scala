// -*- mode: Scala;-*- 
// Filename:    JSONTwistedPair.scala 
// Authors:     lgm                                                    
// Creation:    Sat Oct 30 00:49:45 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package net.liftweb.amqp

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._
import _root_.com.rabbitmq.client._
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ObjectOutputStream

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait AMQPTwistedPair {
  def srcHost : String
  def trgtHost : String

  def amqpSenderToSrc : StdJSONOverAMQPSender
  def amqpReceiverFromTrgt : JSONAMQPListener
}

class JSONAMQPTP(
  override val srcHost : String,
  override val trgtHost : String,
  override val amqpSenderToSrc : StdJSONOverAMQPSender,
  override val amqpReceiverFromTrgt : JSONAMQPListener
) extends AMQPTwistedPair 

class JSONTwistedPair(
  srcHost : String, 
  trgtHost : String
) {
  case object JSONTrgt extends StdJSONOverAMQPSender( trgtHost )
  case object JSONSrc extends JSONAMQPListener( srcHost ) 
}
