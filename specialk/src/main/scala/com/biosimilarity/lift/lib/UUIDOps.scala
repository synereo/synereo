// -*- mode: Scala;-*- 
// Filename:    UUIDOps.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug 10 00:45:45 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib
import java.util.UUID

trait UUIDOps {
  def getUUID(): UUID = UUID.randomUUID() 
  def getUUID(uuid: String) = UUID.fromString(uuid)
}

