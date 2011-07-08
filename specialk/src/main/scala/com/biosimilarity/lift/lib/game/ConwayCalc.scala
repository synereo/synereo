// -*- mode: Scala;-*- 
// Filename:    GenConCalc.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 08:59:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game.conway
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }

// Generators
trait ConwayOps {
  def add (
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : ConwayGame
  def minus (
    g : ConwayGame
  ) : ConwayGame
  def multiply (
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : ConwayGame  
}

trait ConwayConversions {
  implicit def toArabicNumeralDouble(
    g : ConwayGame
  ) : Double = {
    if ( g.left.isEmpty ) {
      if ( g.right.isEmpty ) {
	0
      }
      else {
	- ( ( g.right.map( toArabicNumeralDouble ).min ) + 1 )
      }
    } 
    else {
      if ( g.right.isEmpty ) {
	( 1 + ( g.left.map( toArabicNumeralDouble ).max ) )
      }
      else {
	val l =
	  g.left.map( toArabicNumeralDouble ).max
	val r =
	  g.right.map( toArabicNumeralDouble ).min
	val d = l - r
	l + ( d / 2 )
      }
    }
  }

  implicit def toConwayGame(
    i : Int 
  ) : ConwayGame = {
    def loop( i : Int, acc : ConwayGame ) : ConwayGame = {
      if ( i == 0 ) {
	acc
      }
      else {
	if ( i > 0 ) {
	  loop(
	    i - 1,
	    Game(
	      Set.empty[ConwayGame] ++ List( acc ),
	      Set.empty[ConwayGame]
	    )
	  )
	}
	else {
	  loop(
	    i + 1,
	    Game(
	      Set.empty[ConwayGame],
	      Set.empty[ConwayGame] ++ List( acc )
	    )
	  )
	}
      }
    }
    loop( i, EmptyGame )
  }
}

// Relations
trait ConwayRelations {
  // identity
  def `===` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
  // equality
  def `=G=` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
  // order
  def `G>=` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
}

class ConwayCalculator
extends ConwayOps
with ConwayRelations
with ConwayConversions {  
  // generators
  override def add (
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : ConwayGame = {
    g1 match {
      case EmptyGame => g2
      case _ => {
	g2 match {
	  case EmptyGame => g1
	  case _ => {
	    Game(
	      (
		g1.left.mapf( add( _, g2 ) )
		++
		g2.left.mapf( add( g1, _ ) )
	      ),
	      (
		g1.right.mapf( add( _, g2 ) )
		++
		g2.right.mapf( add( g1, _ ) )
	      )
	    )
	  }
	}
      }
    }
  }
  override def minus (
    g : ConwayGame
  ) : ConwayGame = {    
    Game( g.right.mapf( minus ), g.left.mapf( minus ) )
  }
  override def multiply (
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : ConwayGame = {    
    def mComp(
      gtpl : (
	ConwayGame,
	ConwayGame
      )
    ) = {
      add(
	add(
	  multiply( gtpl._1, g2 ),
	  multiply( g1, gtpl._2 )
	),
	minus( multiply( gtpl._1, gtpl._2 ) )
      )
    }    
   
    Game(
      (
	g1.left.zip( g2.left ).mapf( mComp )
	++ g1.right.zip( g2.right ).mapf( mComp )
      ),
      (
	g1.left.zip( g2.right ).mapf( mComp )
	++ g1.right.zip( g2.left ).mapf( mComp )
      )
    )
  }

  // relations
  // identity
  override def `===` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean = {
    ( ( g1.left.equals( g2.left ) ) && ( g1.right.equals( g2.right ) ) )
  }
  // equality
  override def `=G=` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean = {
    ( `G>=`( g1, g2 ) && `G>=`( g2, g1 ) )
  }
  // order
  override def `G>=` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean = {
    (
      g2.left.forall( ( g2L : ConwayGame ) => ! `G>=`( g2L, g1 ) )
      && g1.right.forall( ( g1R : ConwayGame ) =>  ! `G>=`( g2, g1R ) )
    )
  }
}
