// -*- mode: Scala;-*- 
// Filename:    Scribble.scala 
// Authors:     lgm                                                    
// Creation:    Mon Aug 19 20:32:53 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.scribble
import com.biosimilarity.lift.lib.BasicLogService

import scala.util.continuations._ 

trait Scribble[Resource] {
  def stackTraceToString( t : Throwable ) : String = {
    val errors : java.io.StringWriter = new java.io.StringWriter()
    t.printStackTrace( new java.io.PrintWriter( errors ) )
    errors.toString( )
  }
  def wrapWithCatch(
    sk : Option[Resource] => Unit @suspendable
  ) : Option[Resource] => Unit @suspendable = {
    ( optRsrc ) => {
      try {
	sk( optRsrc )
      }
      catch {
        case t : Throwable => {
          BasicLogService.tweet( "unhandled exception : \n" + stackTraceToString( t ) )
          throw( t )
        }
      }
    }
  }
}
