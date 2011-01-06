// $Id$

package scala.concurrent

trait TaskRunnersBase {
  
  type Task = () => Unit
  type TaskRunner <: AbstractTaskRunner
  
  def createDefaultTaskRunner(): TaskRunner
  implicit val mainTaskRunner: TaskRunner = createDefaultTaskRunner()
  
}