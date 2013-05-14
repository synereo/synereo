// -*- mode: Scala;-*- 
// Filename:    ConcreteUser.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  2 17:01:13 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

package diesel {
  import com.biosimilarity.evaluator.dsl._
  object ConcreteHumanEngagement extends HumanEngagement with Serializable {
    type Duration = ( String, String );
    type Language = String
  }
}
