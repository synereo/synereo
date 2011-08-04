// -*- mode: Scala;-*- 
// Filename:    monadic-amqp-sample.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jul 18 11:36:59 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib._
import scala.util.continuations._

// object AMQPSample extends UUIDOps {
//   val srcHost = "10.0.1.5"
//   val trgtHost = "10.0.1.9"
//   val queueUUID = getUUID

//   val srcSeed = scala.math.round( scala.math.random * 100 ).toInt
//   val trgtSeed = scala.math.round( scala.math.random * 100 ).toInt

//   val srcScope = new AMQPHostScope[Int]( srcHost )
//   val srcQM = new srcScope.AMQPQueueM[Int]( queueUUID.toString, "routeroute" )
//   val srcQueue = srcQM.unit[Int]( srcSeed )

//   val trgtScope = new AMQPHostScope[Int]( trgtHost )
//   val trgtQM = new trgtScope.AMQPQueueM[Int]( queueUUID.toString, "routeroute" )
//   val trgtQueue = trgtQM.unit[Int]( trgtSeed )
// }

object AMQPFunctionsOverJSONTransport {
  import com.thoughtworks.xstream.XStream
  import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

  def exchangeFunction( aFunction : Int => String ) : Unit = {
    // create an AMQP scope
    val fnAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
    val fnQM =
      new fnAMQPScope.AMQPQueueHostExchangeM[String](
	"localhost",
	"myFunctionQueue"
      )
    // get an empty queue
    val fnQ = fnQM.zero[String]
    
    // create an XStream
    val xstrm = new XStream( new JettisonMappedXmlDriver )

    // create a transportable version of the function
    val sendVal = xstrm.toXML( aFunction )
    
    // send the value
    fnQ ! sendVal

    // set up a handler for the queue
    for( fn <- fnQM( fnQ ) ) {
      // fn is a JSON encoding of the function; so we need to convert
      // it back to an Object and then recover the fact that it was a
      // function 
      val f = xstrm.fromXML( fn ).asInstanceOf[Function1[Int,String]];
      // now let's run the function on some arguments
      for( i <- 1 to 5 ) {
	println( "running received function on " + i )
	f( i )
      }
    }
  }
}
