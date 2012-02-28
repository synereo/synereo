// -*- mode: Scala;-*- 
// Filename:    monadic-www.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 28 08:38:28 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib._
import scala.util.continuations._

object WWW extends MonadicEmbeddedJetty[String] with WireTap with Journalist {
  override def tap [A] ( fact : A ) : Unit = { reportage( fact ) }
  def run() : Unit = {
    reset{
      for( request <- WWW.beginService( 8090, "/" ) ) {
	println( "serving request: " + request )
      }
    }
  }
}
