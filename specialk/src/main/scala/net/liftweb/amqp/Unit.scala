// -*- mode: Scala;-*- 
// Filename:    Unit.scala<2> 
// Authors:     lgm                                                    
// Creation:    Mon Dec 13 10:55:37 2010 
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

object TwistedResistor {
  case class Msg( s : String, i : Int, b : Boolean, ref : Option[Msg] )
  val msg1 = Msg( "yo!", 0, false, None )
  val msg2 = Msg( "oy!", 1, true, None )
  def powerUp( trgt : String, src : String ) = {
    val jtp = new JSONAMQPTwisted( trgt, src )
    val ( sndr, rcvr ) = jtp.wires
    sndr.send( msg1 )
  }
}
