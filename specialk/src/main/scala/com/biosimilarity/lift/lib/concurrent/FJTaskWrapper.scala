package jsr166y.forkjoin

object FJTaskWrapper {
  
  // bug in inlining??
  
  @noinline def runOnCurrentThreadOrPool(task: ForkJoinTask[_], pool: ForkJoinPool) = {
    val thread = Thread.currentThread()
    if (thread.isInstanceOf[ForkJoinWorkerThread])
      thread.asInstanceOf[ForkJoinWorkerThread].pushTask(task)
    else
      pool.execute(task)
  }
  
}
