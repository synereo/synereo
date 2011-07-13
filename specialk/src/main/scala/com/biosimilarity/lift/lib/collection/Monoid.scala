// -*- mode: Scala;-*- 
// Filename:    Monoid.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jul 10 13:59:50 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.collection

/* -----------------------------------------------------------------------
 * A monoid is given in terms of the following data:
 *   1) A collection of generators (represented below as the inhabitants of G)
 *   2) A distinguished element of G, called unit
 *   3) A composition operator (represented below as compose)
 * satisfying the following identities: for all g, g1, g2, g3 : G, mg : Monoid[G]
 *   i) mg.compose( mg.unit, g ) == g == mg.compose( g, mg.unit )
 *   ii) mg.compose( mg.compose( g1, g2 ), g3 ) == mg.compose( g1, mg.compose( g2, g3 ) )
 * ---------------------------------------------------------------------- */
trait Monoid[G] {
  def unit : G
  def compose ( g1 : G, g2 : G ) : G
}

class MonoidBoolOr
extends Monoid[Boolean] {
  def unit : Boolean = false
  def compose( g1 : Boolean, g2 : Boolean ) = g1 || g2
}

class MonoidBoolAnd
extends Monoid[Boolean] {
  def unit : Boolean = true
  def compose( g1 : Boolean, g2 : Boolean ) = g1 && g2
}

class MonoidIntPlus
extends Monoid[Int] {
  def unit : Int = 0
  def compose( g1 : Int, g2 : Int ) = g1 + g2
}

class MonoidIntTimes
extends Monoid[Int] {
  def unit : Int = 1
  def compose( g1 : Int, g2 : Int ) = g1 * g2
}

class MonoidString
extends Monoid[String] {
  def unit : String = ""
  def compose( g1 : String, g2 : String ) = g1 + g2
}
