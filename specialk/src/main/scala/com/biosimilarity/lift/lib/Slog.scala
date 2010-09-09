// -*- mode: Scala;-*- 
// Filename:    Slog.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 11:17:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import scala.xml._

trait WireTap {
  def tap [A] ( fact : A ) : Unit
}

trait Journalist {
  trait Verbosity
  case class Twitterer() extends Verbosity
  case class Blogger() extends Verbosity
  case class Luddite() extends Verbosity
  
  def report [A] ( verb : Verbosity )( fact : A ) : Unit = {
    verb match {
      case Twitterer() => {
	val rpt = <report>{fact}</report>
	println( rpt )
      }
      case Blogger() => {
	val rpt = <report>{fact}</report>
	println( rpt )
      }
      case Luddite() => 
    }
  }
}
