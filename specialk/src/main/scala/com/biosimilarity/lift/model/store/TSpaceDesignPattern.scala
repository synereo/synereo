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

trait TupleSpace[Place,Resource]
//       extends MapLike[Place,Resource, This]
{
  self : WireTap with Journalist =>

  type RK = Option[Resource] => Option[Resource]
  type CK = Option[Resource] => Option[Resource]

  def theMeetingPlace : Map[Place,Resource]
  def theWaiters : Map[Place,List[RK]]

  //def self = theMeetingPlace

  // val reportage = report( Luddite() ) _

  def fetch( x : Place ) 
  : Option[Resource] @scala.util.continuations.cpsParam[Option[Resource],Unit]
  = {
    val assay = theMeetingPlace.get( x )
    assay match {
      case sv @ Some( v ) => {
	shift {
	  ( k : RK ) => {
	    theMeetingPlace -= x
	    sv
	  }
	}
      }
      case None => {	
	//reset {
	reportage(
	  (
	    this 
	    + " acquiring continuation to wait for a value "
	  )
	)
	val cv =
	  shift {
	    ( k : RK ) => {	
	      theWaiters( x ) =
		theWaiters.get( x ).getOrElse( Nil ) ++ List( k );
	      
	      //k( None )
	    }
	  }
	reportage(
	  (
	    this
	    + " resuming with value : "
	    + cv
	  )
	)
	theMeetingPlace -= x
	cv
	//}
      }	
    }
  }
  
  def get( x : Place, next : CK ) : Option[Resource] = {
    val stuff = 
      theMeetingPlace.get( x ) match {
	case sv @ Some( v ) => sv
	case None => {	
	  reset {
	    reportage(
	      (
		this
		+ " acquiring continuation to wait for a value "
	      )
	    )
	    val cv =
	      shift {
		( k : RK ) => {	
		  theWaiters( x ) =
		    theWaiters.get( x ).getOrElse( Nil ) ++ List( k );
		  
		  k( None )
		}
	      }
	    reportage(
	      (
		this 
		+ " resuming with value : " + cv
	      )
	    )
	    cv
	  }
	}	
      }    
    stuff match {
      case Some( v ) => next( stuff )
      case nv @ _ => nv
    }
  }

  def get( x : Place ) : Option[Resource] = {
    get( x, ( v ) => { tap( v ); v } )
  }
  
  def put( x : Place, y : Resource ) : Unit = {
    theWaiters.get( x ) match { 
      case Some( k :: ks ) => {
	theWaiters( x ) = ks
	k( Some( y ) );
	()
      }
      case _ => {
	theMeetingPlace( x ) = y
      }
    }
  }
}

object TSpace
       extends TupleSpace[String,String]
       with WireTap
       with Journalist
{
  override val theMeetingPlace = new HashMap[String,String]()
  override val theWaiters = new HashMap[String,List[RK]]()

  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }
  
}
