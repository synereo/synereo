// $Id$

//package scala.concurrent
package com.biosimilarity.lift.lib.concurrent

trait ImmediateTaskRunners extends TaskRunnersBase {
  
  type TaskRunner = ImmediateTaskRunner
  
  class ImmediateTaskRunner extends AbstractSequentialTaskRunner {
    
    def submitTask(f: Task) = f()
    def waitUntilFinished() = {}
    
  }
  
  def createDefaultTaskRunner() = new ImmediateTaskRunner

}
