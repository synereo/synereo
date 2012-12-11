/*
package scala.concurrent

/* User: mgevantmakher
*/


import java.util.concurrent.{Executors, ExecutorService, TimeUnit}

trait ThreadPoolRunners extends TaskRunnersBase
{

  type TaskRunner = ThreadPoolRunner

  def numWorkers: Int = 100 // TODO has to be out of config file

  class ThreadPoolRunner(n: Int) extends AbstractTaskRunner
  {
    val exec: ExecutorService = Executors.newFixedThreadPool(n);

    // todo to evaluate if newCachedThreadPool() IS BETTER

    def submitTask(f: Task)
    {

      exec.submit(new Runnable
      {
        override def run() = f()
      })
    }

    def waitUntilFinished()
    {
      shutdownAndAwaitTermination(5000)
    }


    private def shutdownAndAwaitTermination(expectedShutdownTime: Long)
    {
      // Disable new tasks from being submitted
      this.exec.shutdown();
      try {
        // Wait a while for existing tasks to terminate
        if ( !this.exec.awaitTermination(expectedShutdownTime, TimeUnit.MILLISECONDS) ) {
          // Cancel hard currently executing tasks
          this.exec.shutdownNow();
          // Wait a while for tasks to respond to being cancelled
          if ( !this.exec.awaitTermination(expectedShutdownTime / 2,
            TimeUnit.MILLISECONDS) ) {
          }
        }

      } catch {
        case e: InterruptedException => {
          // (Re-)Cancel if current thread also interrupted
          this.exec.shutdownNow();
          // Preserve interrupt status
          Thread.currentThread().interrupt();
        }
        case e => e.printStackTrace() // TODO check which exceptions can be expected
      }

    }

  }

  def createDefaultTaskRunner() = new ThreadPoolRunner(numWorkers)


}*/
