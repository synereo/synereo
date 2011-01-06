 // $Id$

package scala.concurrent

import java.util.concurrent.atomic._
import jsr166y._
import jsr166y.forkjoin._

trait FJTaskRunners extends TaskRunnersBase {
  
  type TaskRunner = FJTaskRunner
  def numWorkers: Int = java.lang.Runtime.getRuntime().availableProcessors()
  
  class FJTaskRunner(n: Int) extends AbstractTaskRunner {

    val pool = new ForkJoinPool(n)
    
    def submitTask(f:Task) {
      FJTaskWrapper.runOnCurrentThreadOrPool(new RecursiveAction {
        def compute() = try {
          f()
        } catch { case e => e.printStackTrace() }
        // TODO: exception handling
      }, pool)
    }

    def waitUntilFinished() {
//      Thread.sleep(24*60*60*1000)
      pool.awaitTermination(scala.Long.MaxValue,java.util.concurrent.TimeUnit.SECONDS)
      // FIXME: doesn't seem to work (?)
    }

  }
  
  def createDefaultTaskRunner() = new FJTaskRunner(numWorkers)

}
