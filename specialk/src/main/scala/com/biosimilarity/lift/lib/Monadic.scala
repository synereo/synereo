// -*- mode: Scala;-*- 
// Filename:    Monadic.scala 
// Authors:     lgm                                                    
// Creation:    Wed Dec 29 13:57:38 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

//package net.liftweb.amqp
package com.biosimilarity.lift.lib

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan,  _}
//import _root_.scala.actors.Actor

import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait MonadicGenerators {
  self : WireTap with Journalist =>

  trait Generable[+A,-B,+C] {
    def funK : (A => (B @suspendable)) => (C @suspendable)
    
    def foreach( f : (A => B @suspendable) ) : C @suspendable = {
      funK( f )
    }
  }

  case class Generator[+A,-B,+C](
    override val funK : (A => (B @suspendable)) => (C @suspendable)
  ) extends Generable[A,B,C] {   
  }

  // this is really map... fix!
  def mapStream[A,B](
    strmGenerator : Generator[A,Unit,Unit],
    cnvrtr : (A => B)
  ) =
    Generator {
      k : ( B => Unit @suspendable ) =>
	shift {
	  outerK : ( Unit => Unit ) =>
	    reset {
	      for( elem <- strmGenerator ) {
		//tweet( "calling conversion on elem " + elem )
		val trgtElem = cnvrtr( elem )
		//tweet( "calling handler on converted elem " + elem )
		k( trgtElem )
	      }
	      
	      blog( "mapStream returning" )
  	      outerK()
	    }
	}
    }

  def itergen[T]( coll : Iterable[T] ) = 
    Generator {
      gk : ( T => Unit @suspendable ) =>
	val collItr = coll.iterator

	while( collItr.hasNext ) {
	  gk( collItr.next.asInstanceOf[T] )
	}
    }
}

trait MonadicConcurrentGenerators {
  self : MonadicGenerators with FJTaskRunners with WireTap with Journalist =>
    def spawnGen[T]( 
      gen : Generator[T,Unit,Unit]
    ) =
      Generator {
	k : ( T => Unit @suspendable ) =>
	  shift {
	    outerK : ( Unit => Unit ) =>
	      reset {
		for( g <- gen ) {
		  spawn {
		    blog( "before continuation in spawn gen" )
		    k( g )
		    blog( "after continuation in spawn gen" )
		    outerK()
		  }
		}
	      }
	  }
      }
}

trait MonadicWireToTrgtConversion 
{
  self : MonadicGenerators with WireTap with Journalist =>

  type Wire
  type Trgt
    
  def wire2Trgt( wire : Wire ) : Trgt

  def xformAndDispatch(
    msgGenerator : Generator[Wire,Unit,Unit]
  ) = 
    Generator {
      k : ( Trgt => Unit @suspendable ) =>
	shift {
	  outerK : ( Unit => Unit ) =>
	    reset {
	      for( msg <- mapStream[Wire,Trgt]( msgGenerator, wire2Trgt ) ) {		
		blog( "calling dispatch " )
		k( msg )
	      }

	      blog( "dispatch returning" )
  	      outerK()
	    }
	}
    }
}

trait MonadicDispatcher[T] 
extends MonadicGenerators
with FJTaskRunners {
  self : WireTap with Journalist =>

  type Channel
//  type ConnectionParameters
  type Payload  

  def acceptConnections(
    factory : ConnectionFactory,
    host : String,
    port : Int
  ) : Generator[Channel,Unit,Unit]

  def beginService(
    factory : ConnectionFactory,
    host : String,
    port : Int
  ) : Generator[T,Unit,Unit]

  def callbacks(
    channel : Channel
  ) : Generator[Payload,Unit,Unit]

  def readT(
    channel : Channel
  ) : Generator[T,Unit,Unit]

}


