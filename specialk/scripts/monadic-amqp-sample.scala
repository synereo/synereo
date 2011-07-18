// -*- mode: Scala;-*- 
// Filename:    monadic-amqp-sample.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jul 18 11:36:59 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

// Welcome to Scala version 2.9.0.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
// Type in expressions to have them evaluated.
// Type :help for more information.

// scala> import scala.util.continuations._
// import scala.util.continuations._
import scala.util.continuations._

// scala> import com.biosimilarity.lift.lib._
// import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib._

//scala> val sas1 = new StdAMQPScope[Int]()
val sas1 = new StdAMQPScope[Int]()
//sas1: com.biosimilarity.lift.lib.StdAMQPScope[Int] = StdAMQPScope()

//scala> val aqm1 = new sas1.AMQPQueueM[Int]( "gorble", "routeroute" )
val aqm1 = new sas1.AMQPQueueM[Int]( "gorble", "routeroute" )
//aqm1: sas1.AMQPQueueM[Int] = com.biosimilarity.lift.lib.AMQPScope$AMQPQueueM@5f16e402

//scala> val queue1 = aqm1.unit[Int]( 5 ) /* make a queue containing the message 5 */
val queue1 = aqm1.unit[Int]( 5 )
//queue1: sas1.AMQPQueue[Int] = AMQPQueue(gorble,routeroute,Generator(<function1>),Generator(<function1>))

//scala> import aqm1._
//import aqm1._
import aqm1._

//scala> for( msg <- queue1 ) { println( "received: " + msg ) }
for( msg <- queue1 ) { println( "received: " + msg ) }

//scala> received: 5


//scala> reset { for( e <- queue1.sender ) { 10 } }  /* send the message 10 -- note this should be a co-monadic or writer API -- that's in the works */
reset { for( e <- queue1.sender ) { 10 } }

//scala> received: 10
//reset { for( e <- queue1.sender ) { 100 } }
reset { for( e <- queue1.sender ) { 100 } }

//scala> received: 100

//scala> 
