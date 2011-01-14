// -*- mode: Scala;-*- 
// Filename:    HGDBPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Jan 13 16:01:22 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import org.hypergraphdb._

import java.util.UUID

trait HGDBStore {
  self : UUIDOps =>

  def URI : String
  def database : Option[HyperGraph] = {
    try {
      Some( new HyperGraph( URI ) )
    }
    catch {
      case e : Throwable => {
	e.printStackTrace()
	None
      }
    }
  }
}
