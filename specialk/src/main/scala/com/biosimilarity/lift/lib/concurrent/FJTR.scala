// -*- mode: Scala;-*- 
// Filename:    FJTR.scala 
// Authors:     lgm                                                    
// Creation:    Tue Dec  4 14:13:49 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package scala.concurrent

import scala.util.continuations._

import java.util.concurrent.atomic._
import jsr166y._
import jsr166y.forkjoin._

object FJTR extends FJTaskRunners {
  def spawn(ctx: =>(Any @cps[Unit]))(implicit sched: AbstractTaskRunner): Unit = {
    if ( sched == null ) {
      println( ">>>>>>>>>>>>>>>>>>>>>> sched is null >>>>>>>>>>>>>>>>>>>>>>>>>" )
    }
    else {
      sched.submitTask(() => run(ctx))
    }
  }
}

trait FJTaskRunnersX {
  def spawn(ctx: =>(Any @cps[Unit])): Unit = {
    FJTR.spawn( ctx )( FJTR.mainTaskRunner )
  }
}
