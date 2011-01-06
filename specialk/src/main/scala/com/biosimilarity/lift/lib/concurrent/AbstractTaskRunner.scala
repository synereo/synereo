// $Id$

package scala.concurrent

abstract class AbstractTaskRunner {
 
  type Task = () => Unit
  
  def submitTask(code: Task): Unit
  def waitUntilFinished(): Unit
  
}

abstract class AbstractSequentialTaskRunner extends AbstractTaskRunner