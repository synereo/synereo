// -*- mode: Scala;-*- 
// Filename:    manager.scala 
// Authors:     lgm                                                    
// Creation:    Sun Mar 20 17:06:07 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.protegra_ati.agentservices.store._

object Manager {
  def main( args : List[String] ) = {
    PlatformAgentAndUserAgentManager.configureAndListen(
      args( 0 )
    )
  }
}
