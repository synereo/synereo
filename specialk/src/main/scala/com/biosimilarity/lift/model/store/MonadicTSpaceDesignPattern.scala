// -*- mode: Scala;-*- 
// Filename:    TSpaceDesignPattern.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep  6 17:57:30 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import scala.util.continuations._ 
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

trait MonadicTupleSpace[Place,Pattern,Resource]
//       extends MapLike[Place,Resource, This]
extends MonadicGenerators with FJTaskRunners
{
  self : WireTap with Journalist =>

  type RK = Option[Resource] => Unit @suspendable
  //type CK = Option[Resource] => Unit @suspendable

  def theMeetingPlace : Map[Place,Resource]
  def theChannels : Map[Place,Resource]
  def theWaiters : Map[Place,List[RK]]
  def theSubscriptions : Map[Place,List[RK]]

  def matches( ptn : Pattern, place : Place ) : Boolean
  def representative( ptn : Pattern ) : Place

  //def self = theMeetingPlace

  override def itergen[T]( coll : Iterable[T] ) = 
    Generator {
      gk : ( T => Unit @suspendable ) =>
	val collItr = coll.iterator

	while( collItr.hasNext ) {
	  gk( collItr.next )
	}
    }
  

  // val reportage = report( Luddite() ) _
  def mget(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]]
  )( ptn : Pattern )
  : Generator[Option[Resource],Unit,Unit] =
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
	shift {
	  outerk : ( Unit => Unit ) =>
	    reset {
	      val meets =
		channels.filter(
		  ( kv ) => { val ( k, _ ) = kv; matches( ptn, k ) }
		).toList

	      if ( meets.isEmpty )  {
		val place = representative( ptn )
		tweet( "did not find a resource, storing a continuation: " + rk )
		registered( place ) =
		  registered.get( place ).getOrElse( Nil ) ++ List( rk )
		rk( None )
	      }
	      else {
		for(
		  placeNRrsc <- itergen[(Place,Resource)](
		    channels
		  )
		) {
		  val ( place, rsrc ) = placeNRrsc
		  
		  tweet( "found a resource: " + rsrc )		  
		  channels -= place
		  rk( Some( rsrc ) )
		  
		  //shift { k : ( Unit => Unit ) => k() }
		}
	      }
	      tweet( "get returning" )
	      outerk()
	    }
	}
    }

  def get( ptn : Pattern ) =
    mget( theMeetingPlace, theWaiters )( ptn )
  def subscribe( ptn : Pattern ) =
    mget( theChannels, theSubscriptions )( ptn )

  def putPlaces(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    ptn : Pattern,
    rsrc : Resource
  )
  : Generator[Place,Unit,Unit] = {    
    Generator {
      k : ( Place => Unit @suspendable ) => 
	// Are there outstanding waiters at this pattern?    
	val waitlist =
	  registered.keys.filter( ( k ) => matches( ptn, k ) ).toList

	waitlist match {
	  // Yes!
	  case waiter :: waiters => {
	    tweet( "found waiters waiting for a value at " + ptn )
	    val itr = waitlist.toList.iterator
	    while( itr.hasNext ) {
	      k( itr.next )
	    }
	  }
	  // No...
	  case Nil => {
	    // Store the rsrc at a representative of the ptn
	    tweet( "no waiters waiting for a value at " + ptn )
	    channels( representative( ptn ) ) = rsrc
	  }
	}
    }
  }

  def mput(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    publish : Boolean
  )( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {    
    for( wtr <- putPlaces( channels, registered, ptn, rsrc ) ) {
      val Some( rks ) = registered.get( wtr )
      tweet( "waiters waiting for a value at " + wtr + " : " + rks )
      rks match {
	case rk :: rrks => {	
	  if ( publish ) {
	    for( sk <- rks ) {
	      spawn {
		sk( Some( rsrc ) )
	      }
	    }
	  }
	  else {
	    registered( wtr ) = rrks
	    rk( Some( rsrc ) )
	  }
	}
	case Nil => {
	  channels( wtr ) = rsrc	  
	}
      }
    }
        
  }

  def put( ptn : Pattern, rsrc : Resource ) =
    mput( theMeetingPlace, theWaiters, false )( ptn, rsrc )
  def publish( ptn : Pattern, rsrc : Resource ) =
    mput( theChannels, theSubscriptions, true )( ptn, rsrc )
  
}

import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}

object MonadicTSpace
       extends MonadicTupleSpace[String,String,String]
       with WireTap
       with Journalist
{
  override val theMeetingPlace = new HashMap[String,String]()
  override val theChannels = new HashMap[String,String]()
  override val theWaiters = new HashMap[String,List[RK]]()
  override val theSubscriptions = new HashMap[String,List[RK]]()

  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }

  def representative( ptn : String ) : String = {
    ptn
  }

  def matches( ptn : String, place : String ) : Boolean = {
    RegexPtn.matches( ptn, place ) || RegexPtn.matches( place, ptn )
  }
  
}
