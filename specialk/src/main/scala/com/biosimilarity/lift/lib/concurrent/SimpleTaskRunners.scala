// $Id$

package scala.concurrent

import scala.collection.mutable.Queue

trait SimpleTaskRunners extends TaskRunnersBase {
  
  type TaskRunner = SimpleTaskRunner
  
  class SimpleTaskRunner extends AbstractSequentialTaskRunner {
    val runq = new Queue[Task]()

    val maxNest = 10//150
    var curNest = 0

    def submitTask(f:Task) {
      if (curNest < maxNest) {
        curNest += 1
        f();
      } else {
        curNest = 0
        runq += f
      }
    }

    def waitUntilFinished() {
      while(!runq.isEmpty) {
        val k = runq.dequeue();
        k()
      }
    }

  }
  
  def createDefaultTaskRunner() = new SimpleTaskRunner

}