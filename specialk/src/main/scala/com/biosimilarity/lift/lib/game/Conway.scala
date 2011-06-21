// -*- mode: Scala;-*- 
// Filename:    GenCon.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 10 11:50:40 2011 
// Copyright:   Not supplied 
// Description: 
//
// A simple implementation of Conway for expository purposes.
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game.conway
import com.biosimilarity.lift.lib.collection._

trait ConwayGame {
  def left : DeCantor[ConwayGame]
  def right : DeCantor[ConwayGame]  
}

trait ConwayRenderer {
  self : ConwayGame =>

  def openBrace : String = "{"
  def closeBrace : String = "}"

  def componentBound : Int = 10

  override def toString : String = {
    def compStr( comp : List[ConwayGame] ) = {
      comp match {
	case Nil => ""
	case l :: ls => {
	  val s = l + ""
	  ( s /: ls )(
	    ( acc, e ) => {
	      acc + "," + ( e + "" )
	    }
	  )
	}
      }
    }

    (
      openBrace
      + compStr( left.toList )
      + "|"
      + compStr( right.toList )
      + closeBrace
    )
  }
}

case object EmptyGame
extends ConwayGame
with ConwayRenderer
{
  override def left : DeCantor[ConwayGame] =
    DeCantor.empty[ConwayGame]
  override def right : DeCantor[ConwayGame] = 
    DeCantor.empty[ConwayGame]
}

case class Game(
  left : DeCantor[ConwayGame],
  right : DeCantor[ConwayGame]
) extends ConwayGame 
     with ConwayRenderer

