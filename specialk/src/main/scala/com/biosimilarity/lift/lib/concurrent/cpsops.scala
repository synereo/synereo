package scala.concurrent

import scala.util.continuations._
//import scala.continuations.ControlContext._

object cpsops {
  
  def spawn(ctx: =>(Any @cps[Unit]))(implicit sched: AbstractTaskRunner): Unit = {
    if ( sched == null ) {
      println( ">>>>>>>>>>>>>>>>>>>>>> sched is null >>>>>>>>>>>>>>>>>>>>>>>>>" )
    }
    else {
      sched.submitTask(() => run(ctx))
    }
  }
  
  
  
  
}
