// -*- mode: Scala;-*- 
// Filename:    monadic-amqp-sample.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jul 18 11:36:59 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import scala.util.continuations._
import com.biosimilarity.lift.lib._

object AMQPSample extends UUIDOps {
  val srcHost = "10.0.1.5"
  val trgtHost = "10.0.1.9"
  val queueUUID = getUUID

  val srcSeed = scala.math.round( scala.math.random * 100 ).toInt
  val trgtSeed = scala.math.round( scala.math.random * 100 ).toInt

  val srcScope = new AMQPHostScope[Int]( srcHost )
  val srcQM = new srcScope.AMQPQueueM[Int]( queueUUID.toString, "routeroute" )
  val srcQueue = srcQM.unit[Int]( srcSeed )

  val trgtScope = new AMQPHostScope[Int]( trgtHost )
  val trgtQM = new trgtScope.AMQPQueueM[Int]( queueUUID.toString, "routeroute" )
  val trgtQueue = trgtQM.unit[Int]( trgtSeed )
}
