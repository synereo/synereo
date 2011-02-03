// -*- mode: Scala;-*- 
// Filename:    DirectedLinks.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb  2 15:50:03 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import java.net.URI

// D_irected L_ink F_actory
// what type constraint can we impose to insist Linker have a ctor of
  // two arguments both of type String?

object AgentURIDefaults {
  trait Direction
  case class A2B() extends Direction
  case class B2A() extends Direction

  def scheme : String = "agent"
  def path : String = "/invitation"
  def fragment : String = ""
  implicit def stringToURI( s : String ) : URI = {
    new URI( scheme, s, path, fragment )
  }  
}




