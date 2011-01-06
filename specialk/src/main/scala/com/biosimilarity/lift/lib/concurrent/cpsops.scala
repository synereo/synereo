package scala.concurrent

import scala.util.continuations._
//import scala.continuations.ControlContext._

object cpsops {
  
  def spawn(ctx: =>(Any @cps[Unit]))(implicit sched: AbstractTaskRunner): Unit = {
    sched.submitTask(() => run(ctx))
  }
  
  
  
  
}
