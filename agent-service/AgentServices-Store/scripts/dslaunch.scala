// -*- mode: Scala;-*- 
// Filename:    dslaunch.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jun 18 11:40:13 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.evaluator.distribution.diesel._

object Go extends Serializable {
  def now() = {
    val a1 = new Array[String]( 1 )
    a1( 0 ) = "config=eval.conf"
    Server.mainEntryPoint( a1 )
    Server.engine()
  }
  def go() = {
    Server.run()
  }
}
