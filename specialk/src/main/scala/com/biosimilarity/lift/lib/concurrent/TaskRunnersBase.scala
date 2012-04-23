// $Id$

package scala.concurrent

trait TaskRunnersBase {
  
  type Task = () => Unit
  type TaskRunner <: AbstractTaskRunner
  
  def createDefaultTaskRunner(): TaskRunner
  @transient implicit lazy val mainTaskRunner: TaskRunner = createDefaultTaskRunner()
  
}
