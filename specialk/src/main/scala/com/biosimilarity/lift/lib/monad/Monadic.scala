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
//import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan,  _}

import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait MonadicGenerators {
  //self : WireTap =>

  trait Generable[+A,-B,+C] {
    def funK : (A => (B @suspendable)) => (C @suspendable)
    
    def foreach( f : (A => B @suspendable) ) : C @suspendable = {
      funK( f )
    }
    def map [Aprime] ( g : A => Aprime ) : Generable[Aprime,B,C]
    def mapSrc [Aprime] ( g : A => Aprime ) : Generable[Aprime,B,C]
    def mapTrgt [Bprime] ( g : Bprime => B ) : Generable[A,Bprime,C]
  
    def flatMap [Aprime >: A, Bprime <: B, Cprime >: C] (
      g : Aprime => Generable[Aprime,Bprime,Cprime]
    ) : Generable[Aprime,Bprime,Cprime]

    def filter( pred : A => Boolean ) : Generable[Option[A],B,C] 

    def map [Aprime] ( f : ( Aprime => B @suspendable ) )(
      g : A => Aprime
    ) : (C @suspendable) = {
      map( g ).funK( f )
    }
    def mapSrc [Aprime] ( f : ( Aprime => B @suspendable ) )(
      g : A => Aprime
    ) : (C @suspendable) = {
      mapSrc( g ).funK( f )
    }
    def mapTrgt [Bprime] ( f : ( A => Bprime @suspendable ) )(
      g : Bprime => B
    ) : (C @suspendable) = {
      mapTrgt( g ).funK( f )
    }
  
    def flatMap [Aprime >: A, Bprime <: B, Cprime >: C] (
      f : ( Aprime => Bprime @suspendable )
    )(
      g : Aprime => Generable[Aprime,Bprime,Cprime]
    ) : (Cprime @suspendable) = {
      flatMap( g ).funK( f )
    }
    
    def filter ( f : ( Option[A] => B @suspendable ) )(
      pred : A => Boolean
    ) : (C @suspendable) = {
      filter( pred ).funK( f )
    }
    
  }

  case class Generator[+A,-B,+C](
    override val funK : (A => (B @suspendable)) => (C @suspendable)
  ) extends Generable[A,B,C] {               
    override def map [Aprime] (
      g : A => Aprime
    ) : Generable[Aprime,B,C] = mapSrc[Aprime]( g )
    override def mapSrc [Aprime] (
      g : A => Aprime
    ) : Generable[Aprime,B,C] = {
      Generator {
	k : ( Aprime => B @suspendable ) =>
	  funK( ( a : A ) => k( g( a ) ) )
      }
    }
    override def mapTrgt [Bprime] (
      g : Bprime => B
    ) : Generable[A,Bprime,C] = {
      Generator {
	k : ( A => Bprime @suspendable ) =>
	  funK( ( a : A ) => g( k( a ) ) )
      }
    }

    override def flatMap [Aprime >: A, Bprime <: B, Cprime >: C] (
      g : Aprime => Generable[Aprime,Bprime,Cprime]
    ) : Generable[Aprime,Bprime,Cprime] = {
      Generator {
	k : ( Aprime => Bprime @suspendable ) => {
	  // this is a hack!
	  // we really need to calculate a coend to eliminate the
	  // dependency on A
	  var rA : Option[Aprime] = None 
	  funK( ( a : A ) => { rA = Some( a ); k( a ) } )
	  rA match {
	    case Some( a ) => g( a ).funK( k )
	    case _ => throw new Exception( "Should never get here!" )
	  }
	}
      }
    }

    override def filter(
      pred : A => Boolean
    ) : Generable[Option[A],B,C] = {
      mapSrc[Option[A]](
	( a : A ) => { if ( pred( a ) ) { Some( a ) } else { None } }
      )
    }
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
		//BasicLogService.tweet( "calling conversion on elem " + elem )
		val trgtElem = cnvrtr( elem )
		//BasicLogService.tweet( "calling handler on converted elem " + elem )
		k( trgtElem )
	      }
	      
	      //BasicLogService.blog( "mapStream returning" )
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
  self : MonadicGenerators
    //with FJTaskRunnersX
    with ThreadPoolRunnersX
    with WireTap =>
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
		    BasicLogService.blog( "before continuation in spawn gen" )
		    k( g )
		    BasicLogService.blog( "after continuation in spawn gen" )
		    outerK()
		  }
		}
	      }
	  }
      }
}

trait WireToTrgtConversion {
  type Wire
  type Trgt
    
  def wire2Trgt( wire : Wire ) : Trgt
  def trgt2Wire( trgt : Trgt ) : Wire  
}

trait MonadicWireToTrgtConversion 
{
  self : MonadicGenerators
       with WireToTrgtConversion with WireTap =>

  def xformAndDispatch(
    msgGenerator : Generator[Wire,Unit,Unit]
  ) = 
    Generator {
      k : ( Trgt => Unit @suspendable ) =>
	shift {
	  outerK : ( Unit => Unit ) =>
	    reset {
	      for( msg <- mapStream[Wire,Trgt]( msgGenerator, wire2Trgt ) ) {		
		BasicLogService.blog( "calling dispatch " )
		k( msg )
	      }

	      BasicLogService.blog( "dispatch returning" )
  	      outerK()
	    }
	}
    }
}

trait MonadicDispatcher[T] 
  extends MonadicGenerators
  with ThreadPoolRunnersX
  //with FJTaskRunnersX
{
  self : WireTap =>

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

  def serve [T] (
    factory : ConnectionFactory,
    host : String,
    port : Int,
    exchange : String
  ) : Generator[T,Unit,Unit]

  def callbacks(
    channel : Channel
  ) : Generator[Payload,Unit,Unit]

  def readT(
    channel : Channel
  ) : Generator[T,Unit,Unit]

  def read [T] (
    channel : Channel,
    exchange : String
  ) : Generator[T,Unit,Unit]

}


trait MonadicDispatcherScope[T] {
  type MDS[A] <: MonadicDispatcher[A]
  @transient val theMDS : MDS[T] = protoMDS[T]
  def protoMDS[A] : MDS[A]
}

