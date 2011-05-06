// -*- mode: Scala;-*- 
// Filename:    rabbit-connectivity.scala 
// Authors:     lgm                                                    
// Creation:    Tue Apr 19 13:10:19 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.util.continuations._ 

object RabbitMQConnectivityTest {
  import AMQPDefaults._
  import MonadicAMQPUnitTest._
  def checkConnectivity( a : String, b : String ) = {
    var msgCount = 0
    val sma1 = SMJATwistedPair[Msg]( a, b )
    sma1.jsonSender( "myFavoriteQueue" )
    sma1.jsonDispatcher(
      "myFavoriteQueue",
      ( x ) => {
	println( "received : " + x )
	msgCount += 1
      }
    )

    val msgs = msgStrm.take( 100 )
    
    for( i <- 0 to 9 ) { sma1.send( msgs( i ) ) }
      
    while ( msgCount < 10 ) {
      Thread.sleep( 100 )
    }
  }      
}
