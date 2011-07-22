// -*- mode: Scala;-*- 
// Filename:    AMQPTwistedPairMnd.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 10:54:21 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan, _}
import _root_.scala.actors.Actor

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import _root_.java.io.ObjectInputStream
import _root_.java.io.ObjectOutputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.io.ByteArrayOutputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait AMQPTwistedPairScope[T] {
  self : AMQPBrokerScope[T] with MonadicDispatcherScope[T] =>

    def src : URI
  def trgt : URI

  def srcHost : String = src.getHost
  def trgtHost : String = trgt.getHost
  def srcPort : Int = src.getPort
  def trgtPort : Int = trgt.getPort
  
  case class TwistedQueuePair[T](
    srcQ : AMQPQueue[T],
    trgtQ : AMQPQueue[T]
  )
  
  class TwistedQueuePairM[A](
    override val host : String,
    override val port : Int,
    val exchange : String,
    val routingKey : String
  ) extends StdMonadicAMQPDispatcher[A]( host, port )
  with ForNotationShiv[TwistedQueuePair,A] 
  with ForNotationApplyShiv[TwistedQueuePair,A]
  with BMonad[TwistedQueuePair]
  with MonadPlus[TwistedQueuePair]
  with MonadFilter[TwistedQueuePair] {
    override type ForNotationTrampoline[A] = TwistedPairCell[A]
    case class TwistedPairCell[A](
      pair : TwistedQueuePair[A]
    ) extends SCell( pair ) {
      override def foreach ( f : A => Unit ) : Unit = {
	reset { for( msg <- pair.srcQ.dispatcher ) { f( msg ) } } ;
	()
      }
    }    

    def sender [A] ( 
      host : String,
      port : Int,
      exchange : String,
      routingKey : String
    ) : theMDS.Generator[Unit, A, Unit] = {
      theMDS.Generator {
	k : ( Unit => A @suspendable ) => {
	  for( channel <- senderChannel( host, port, exchange, routingKey ) ) {
	    spawn {
	      val qname = ( exchange + "_queue" )
		channel.exchangeDeclare( exchange, "direct" )
	      channel.queueDeclare( qname, true, false, false, null );
	      channel.queueBind( qname, exchange, routingKey )
	      
	      val bytes = new ByteArrayOutputStream
	      val store = new ObjectOutputStream(bytes)
	      store.writeObject( k() )
	      store.close
	      channel.basicPublish(
		exchange,
		routingKey,
		null,
		bytes.toByteArray
	      )	    
	    }
	  }
	}
      }
    }
    
    def senderChannel [A] ( 
      host : String,
      port : Int,
      exchange : String,
      routingKey : String
    ) : theMDS.Generator[Channel, Unit, Unit] = {
      theMDS.Generator {
	k : ( Channel => Unit @suspendable ) => {
	  val conn = factory.newConnection( Array { new Address(host, port) } )
	  val channel = conn.createChannel()
	  k( channel )
	}
      }
    }

    override def apply [S] (
      pair : TwistedQueuePair[S]
    ) : ForNotationTrampoline[S] = {
      TwistedPairCell[S]( pair )
    }

    override def unit [S] ( s : S ) : TwistedQueuePair[S] = {
      val rslt = zero[S]
      spawn {
	reset { for( e <- rslt.trgtQ.sender ) { s } }
      }
      rslt
    }
    override def bind [S,T] (
      pairS : TwistedQueuePair[S],
      f : S => TwistedQueuePair[T]
    ) : TwistedQueuePair[T] = {
      val acc = zero[T]
      spawn {
	reset {
	  for( s <- pairS.srcQ.dispatcher ) {
	    val pairT = f( s )
	    spawn {
	      reset {
		for( t <- pairT.srcQ.dispatcher ) {
		  for( e <- acc.trgtQ.sender ) { t }
		}
	      }
	    }		
	  }
	}
      }
      acc
    }
    override def zero [A] : TwistedQueuePair[A] = {
      TwistedQueuePair[A](
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[A]( factory, srcHost, srcPort, exchange ),
	  sender( srcHost, srcPort, exchange, routingKey )
	),
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[A]( factory, trgtHost, trgtPort, exchange ),
	  sender( trgtHost, trgtPort, exchange, routingKey )
	)
      )
    }
    override def plus [A] (
      pairA1 : TwistedQueuePair[A],
      pairA2 : TwistedQueuePair[A]
    ) : TwistedQueuePair[A] = {
      val pairA12 = zero[A]
      spawn {
	reset {
	  for( a1 <- pairA1.srcQ.dispatcher ) {
	    for( e <- pairA12.trgtQ.sender ) { a1 }
	  }
	}
      }
      spawn {
	reset {
	  for( a2 <- pairA2.srcQ.dispatcher ) {
	    for( e <- pairA12.trgtQ.sender ) { a2 }
	  }
	}
      }
      pairA12
    }
    override def mfilter [S] (
      pairS : TwistedQueuePair[S],
      pred : S => Boolean
    ) : TwistedQueuePair[S] = {
      val rslt = zero[S]
      spawn {
	reset {
	  for( s <- pairS.srcQ.dispatcher ) {
	    if ( pred( s ) ) {
	      for( e <- rslt.trgtQ.sender ) { s }
	    }
	  }
	}
      }
      rslt
    }

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : TwistedQueuePairM[A] => {
	  (
	    host.equals( that.host )
	    && port.equals( that.port )
	    && exchange.equals( that.exchange )
	    && routingKey.equals( that.routingKey )
	  )
	}
	case _ => false
      }
    }
    
    override def hashCode( ) : Int = {
      (
	( 37 * host.hashCode )
	+ ( 37 * port.hashCode )
	+ ( 37 * exchange.hashCode )
	+ ( 37 * routingKey.hashCode )
      )
    }
  }
}
