// -*- mode: Scala;-*- 
// Filename:    TSpaceDesignPattern.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep  6 17:57:30 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import scala.util.continuations._ 
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

trait MonadicTupleSpace[Place,Pattern,Resource]
//       extends MapLike[Place,Resource, This]
extends MonadicGenerators
{
  self : WireTap with Journalist =>

  type RK = Option[Resource] => Unit @suspendable
  type CK = Option[Resource] => Unit @suspendable

  def theMeetingPlace : Map[Place,Resource]
  def theWaiters : Map[Place,List[RK]]

  def matches( ptn : Pattern, place : Place ) : Boolean
  def representative( ptn : Pattern ) : Place

  //def self = theMeetingPlace

  // val reportage = report( Luddite() ) _
  def get( ptn : Pattern ) =
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
	shift {
	  outerk : ( Unit => Unit ) =>
	    reset {
	      for(
		placeNRrsc <- itergen[(Place,Resource)](
		  theMeetingPlace
		)
	      ) {
		val ( place, rsrc ) = placeNRrsc
		matches( ptn, place ) match {
		  case true => {
		    rk( Some( rsrc ) )
		  }
		  case false => {
		    theWaiters( place ) =
		      theWaiters.get( place ).getOrElse( Nil ) ++ List( rk )
		    rk( None )
		  }
		}
	      }
	      blog( "get returning" )
	      outerk()
	    }
	}
    }

  def putPlaces( ptn : Pattern, rsrc : Resource )
  : Generator[Place,Unit,Unit] = {    
    Generator {
      k : ( Place => Unit @suspendable ) => 
	// Are there outstanding waiters at this pattern?    
	val waitlist =
	  theWaiters.keys.filter( ( k ) => matches( ptn, k ) ).toList

	waitlist match {
	  // Yes!
	  case waiter :: waiters => {
	    val itr = waitlist.toList.iterator
	    while( itr.hasNext ) {
	      k( itr.next )
	    }
	  }
	  // No...
	  case Nil => {
	    // Store the rsrc at a representative of the ptn
	    theMeetingPlace( representative( ptn ) ) = rsrc
	  }
	}
    }
  }

  def put( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {    
    for( wtr <- putPlaces( ptn, rsrc ) ) {
      val Some( rks ) = theWaiters.get( wtr )
      rks match {
	case rk :: rrks => {
	  theWaiters( wtr ) = rrks
	  rk( Some( rsrc ) )
	}
	case Nil => {
	  theMeetingPlace( wtr ) = rsrc	  
	}
      }
    }
        
  }
  
}

import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}

object MonadicTSpace
       extends MonadicTupleSpace[String,String,String]
       with WireTap
       with Journalist
{
  override val theMeetingPlace = new HashMap[String,String]()
  override val theWaiters = new HashMap[String,List[RK]]()

  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }

  def representative( ptn : String ) : String = {
    ptn
  }

  def matches( ptn : String, place : String ) : Boolean = {
    RegexPtn.matches( ptn, place )
  }
  
}
