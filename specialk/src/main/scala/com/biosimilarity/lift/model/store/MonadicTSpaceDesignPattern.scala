// -*- mode: Scala;-*- 
// Filename:    TSpaceDesignPattern.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep  6 17:57:30 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.design

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import scala.util.continuations._ 
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

trait MonadicTupleSpaceDP[Place,Pattern,Resource]
//       extends MapLike[Place,Resource, This]
extends FJTaskRunners
{
  type RK = Option[Resource] => Unit @suspendable
  //type CK = Option[Resource] => Unit @suspendable
  type Substitution <: Function1[Resource,Option[Resource]]

  case class IdentitySubstitution( )
       extends Function1[Resource,Option[Resource]] {
	 override def apply( rsrc : Resource ) = Some( rsrc )
       }

  case class PlaceInstance(
    place : Place,
    stuff : Either[Resource,List[RK]],
    subst : Substitution
  )

  def theMeetingPlace : Map[Place,Resource]
  def theChannels : Map[Place,Resource]
  def theWaiters : Map[Place,List[RK]]
  def theSubscriptions : Map[Place,List[RK]]

  def fits( ptn : Pattern, place : Place ) : Boolean
  def fitsK( ptn : Pattern, place : Place ) : Option[Substitution]
  def representative( ptn : Pattern ) : Place  

  //def self = theMeetingPlace  
  
  def locations(
    map : Either[Map[Place,Resource],Map[Place,List[RK]]],
    ptn : Pattern
  ) : List[PlaceInstance] = {
    def lox[Trgt,ITrgt](
      m : Map[Place,Trgt],
      inj : Trgt => ITrgt
    ) : List[(Place,ITrgt,Substitution)]
    = {
      ( ( Nil : List[(Place,ITrgt,Substitution)] ) /: m )( 
	{ 
	  ( acc, kv ) => {
	    val ( k, v ) = kv
	    fitsK( ptn, k ) match {
	      case Some( s ) => acc ++ List[(Place,ITrgt,Substitution)]( ( k, inj( v ), s ) )
	      case None => acc
	    }
	  }
	}
      )
    }
    val triples =
      map match {
	case Left( m ) => {
	  lox[Resource,Either[Resource,List[RK]]](
	    m, ( r ) => Left[Resource,List[RK]]( r )
	  )
	}
	case Right( m ) => {
	  lox[List[RK],Either[Resource,List[RK]]](
	    m, ( r ) => Right[Resource,List[RK]]( r )
	  )
	}
      }
    triples.map(
      ( t ) => { 
	val ( p, e, s ) = t
	PlaceInstance( p, e, s )
      }
    )
  }

  // rename to mget and the function below to mgetCorrect to generate
  // runtime error with a call like
  // reset {
  //   for( e <- MonadicRegexTSpace.get( "a.*" ) ) {
  //    println( "received: " + e )
  //   } 
  // }
  def mget(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : Boolean
  )( ptn : Pattern )
  : Generator[Option[Resource],Unit,Unit] =
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
	shift[Unit,Unit,Unit] {
	  outerk : ( Unit => Unit ) =>
	    reset[Unit,Unit] {
	      val map = Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
	      val meets = locations( map, ptn )

	      if ( meets.isEmpty )  {
		val place = representative( ptn )
		//println( "did not find a resource, storing a continuation: " + rk )
		registered( place ) =
		  registered.get( place ).getOrElse( Nil ) ++ List( rk )
		//rk( None )
		//println( "get suspending" )
		//outerk()
	      }
	      else {
		for(
		  placeNRrscNSubst <- itergen[PlaceInstance](
		    meets
		  )
		) {
		  val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
		  
		  //println( "found a resource: " + rsrc )		  
		  if ( consume ) {
		    channels -= place
		  }
		  rk( s( rsrc ) )
		  //println( "get returning" )
		  outerk()
		  //shift { k : ( Unit => Unit ) => k() }
		}
	      }
	      //println( "get returning" )
	      //outerk()
	    }
	}
    }

  // rename to mgetCorrect and the function above to mget to generate
  // runtime error with a call like
  // reset {
  //   for( e <- MonadicRegexTSpace.get( "a.*" ) ) {
  //    println( "received: " + e )
  //   } 
  // }
  def mgetCorrect(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : Boolean
  )( ptn : Pattern )
  : Generator[Option[Resource],Unit,Unit] =
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
	shift {
	  outerk : ( Unit => Unit ) =>
	    reset {
	      val map = Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
	      val meets = locations( map, ptn )

	      if ( meets.isEmpty )  {
		val place = representative( ptn )
		//println( "did not find a resource, storing a continuation: " + rk )
		registered( place ) =
		  registered.get( place ).getOrElse( Nil ) ++ List( rk )
		rk( None )
	      }
	      else {
		for(
		  placeNRrscNSubst <- itergen[PlaceInstance](
		    meets
		  )
		) {
		  val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
		  
		  //println( "found a resource: " + rsrc )		  
		  if ( consume ) {
		    channels -= place
		  }
		  rk( s( rsrc ) )
		  
		  //shift { k : ( Unit => Unit ) => k() }
		}
	      }
	      //println( "get returning" )
	      outerk()
	    }
	}
    }

  def get( ptn : Pattern ) =
    mget( theMeetingPlace, theWaiters, true )( ptn )
  def fetch( ptn : Pattern ) =
    mget( theMeetingPlace, theWaiters, false )( ptn )
  def subscribe( ptn : Pattern ) =
    mget( theChannels, theSubscriptions, true )( ptn )

  def putPlaces(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    ptn : Pattern,
    rsrc : Resource
  )
  : Generator[PlaceInstance,Unit,Unit] = {    
    Generator {
      k : ( PlaceInstance => Unit @suspendable ) => 
	// Are there outstanding waiters at this pattern?    
	val map = Right[Map[Place,Resource],Map[Place,List[RK]]]( registered )
	val waitlist = locations( map, ptn )

	waitlist match {
	  // Yes!
	  case waiter :: waiters => {
	    //println( "found waiters waiting for a value at " + ptn )
	    val itr = waitlist.toList.iterator	    
	    while( itr.hasNext ) {
	      k( itr.next )
	    }
	  }
	  // No...
	  case Nil => {
	    // Store the rsrc at a representative of the ptn
	    //println( "no waiters waiting for a value at " + ptn )
	    channels( representative( ptn ) ) = rsrc
	  }
	}
    }
  }

  def mput(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : Boolean
  )( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {    
    for( placeNRKsNSubst <- putPlaces( channels, registered, ptn, rsrc ) ) {
      val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
      //println( "waiters waiting for a value at " + wtr + " : " + rks )
      rks match {
	case rk :: rrks => {	
	  if ( consume ) {
	    for( sk <- rks ) {
	      spawn {
		sk( s( rsrc ) )
	      }
	    }
	  }
	  else {
	    registered( wtr ) = rrks
	    rk( s( rsrc ) )
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
	      
	      //println( "mapStream returning" )
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

package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}

object MonadicRegexTSpace
       extends MonadicTupleSpaceDP[String,String,String]
{

  override type Substitution = IdentitySubstitution

  override val theMeetingPlace = new HashMap[String,String]()
  override val theChannels = new HashMap[String,String]()
  override val theWaiters = new HashMap[String,List[RK]]()
  override val theSubscriptions = new HashMap[String,List[RK]]()

  def representative( ptn : String ) : String = {
    ptn
  }

  def fits( ptn : String, place : String ) : Boolean = {
    RegexPtn.matches( ptn, place ) || RegexPtn.matches( place, ptn )
  }

  def fitsK(
    ptn : String,
    place : String
  ) : Option[Substitution] = {
    //println( "in fitsK on " + this )
    if ( fits( ptn, place ) ) {
      Some( IdentitySubstitution() )
    }
    else {
      None
    }
  }
  
}


}
