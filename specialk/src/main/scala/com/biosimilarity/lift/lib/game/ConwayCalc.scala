// -*- mode: Scala;-*- 
// Filename:    GenConCalc.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 08:59:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game.conway
import com.biosimilarity.lift.lib.collection._

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

// Relations
trait ConwayRelations {
  def `=G=` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
  def `|G>` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
  def `||` ( 
    g1 : ConwayGame,
    g2 : ConwayGame
  ) : Boolean
}

class ConwayCalculator extends ConwayOps {
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
}
