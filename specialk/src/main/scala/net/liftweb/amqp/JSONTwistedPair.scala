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

  def amqpSenderToTrgt : StdJSONOverAMQPSender
  def amqpReceiverFromSrc : JSONAMQPListener

  def wires : ( StdJSONOverAMQPSender, JSONAMQPListener ) = {
    ( amqpSenderToTrgt, amqpReceiverFromSrc )
  }
}

abstract class JSONAMQPTP(
  override val srcHost : String,
  override val trgtHost : String,
  @transient override val amqpSenderToTrgt : StdJSONOverAMQPSender,
  @transient override val amqpReceiverFromSrc : JSONAMQPListener
) extends AMQPTwistedPair 

class JSONAMQPTwisted(
  override val srcHost : String,
  override val trgtHost : String  
) extends AMQPTwistedPair {
  @transient var _amqpSenderToTrgt : Option[StdJSONOverAMQPSender] = None
  @transient var _amqpReceiverFromSrc : Option[JSONAMQPListener] = None

  override def amqpSenderToTrgt : StdJSONOverAMQPSender = {
    _amqpSenderToTrgt match { 
      case Some( sndr ) => sndr
      case None => {
	val sndr = new StdJSONOverAMQPSender( trgtHost )
	_amqpSenderToTrgt = Some( sndr )
	sndr 
      }
    }
  }

  override def amqpReceiverFromSrc : JSONAMQPListener = {
    _amqpReceiverFromSrc match {
      case Some( rcvr ) => rcvr 
      case None => {
	val rcvr = new JSONAMQPListener( srcHost )
	_amqpReceiverFromSrc = Some( rcvr )
	rcvr
      }
    }
  }
}

// Both of these fail!
// class JSONAMQPTwisted(
//   override val srcHost : String,
//   override val trgtHost : String
// ) extends AMQPTwistedPair {
//   override lazy val amqpSenderToTrgt : StdJSONOverAMQPSender =
//     new StdJSONOverAMQPSender( trgtHost )
//   override lazy val amqpReceiverFromSrc : JSONAMQPListener =
//     new JSONAMQPListener( srcHost )
// }

// class JSONTwistedPair(
//   srcHost : String, 
//   trgtHost : String
// ) {
//   case object JSONTrgt extends StdJSONOverAMQPSender( trgtHost )
//   case object JSONSrc extends JSONAMQPListener( srcHost ) 
// }
