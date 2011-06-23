// -*- mode: Scala;-*- 
// Filename:    GenConNums.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 19:58:08 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }

case object TheNaturalsViaConway {
  lazy val theCalculator : GenConCalculator[String] =
    new GenConCalculator[String]()

  lazy val theOne : GeneralizedConwayGame[String] = 
    new GenConGame(
      Set( Right( EmptyGenConGame ) ),
      Set.empty
    )

  lazy val theNumbers : Stream[GeneralizedConwayGame[String]] =    
    List( EmptyGenConGame ).toStream.append(
      theNumbers.map(
	( gN ) => theCalculator.add( theOne, gN )
      )
    )
}
