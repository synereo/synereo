// -*- mode: Scala;-*- 
// Filename:    search.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 14 04:34:22 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.zipper

import scala.collection.mutable.Map
import scala.collection.mutable.MapLike

trait ZipperMap[A]
extends Map[Location[A],Tree[A]] {
}
