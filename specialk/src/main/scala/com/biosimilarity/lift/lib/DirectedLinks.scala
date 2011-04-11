// -*- mode: Scala;-*- 
// Filename:    DirectedLinks.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb  2 15:50:03 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import java.net.URI

object SpecialKURIDefaults {
  def scheme : String = "specialk"
  def path : String = "/connection"
  def fragment : String = ""
  implicit def stringToURI( s : String ) : URI = {
    new URI( scheme, s, path, fragment )
  }
  implicit def symbolToURI( s : Symbol ) : URI = {
    new URI( scheme, s.toString.replace( "'", "" ), path, fragment )
  }
}




