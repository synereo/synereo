// $Id$

package scala.concurrent

trait ThreadTaskRunners extends TaskRunnersBase {
  
  type TaskRunner = ThreadTaskRunner
  
  class ThreadTaskRunner extends AbstractTaskRunner {

    def submitTask(f:Task) {
      new java.lang.Thread() {
        override def run() = f()
      }.start()
    }

    def waitUntilFinished() {
      // TODO: join running threads
    }

  }
  
  def createDefaultTaskRunner() = new ThreadTaskRunner

}