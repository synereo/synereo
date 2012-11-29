package com.protegra_ati.agentservices.core.util

/* User: mgevantmakher
*/

object ThreadRenamer
{

  def rename(p: => Unit)(newName: String): Unit =
  {
/*
    val reallyName = Thread.currentThread().getName

    try {
      // TODO println has to be deleted !!!
      System.err.println(System.currentTimeMillis +  " before, will change name from " + reallyName + " to " + newName)
      val t = Thread.currentThread()
      if ( reallyName.equals(t.getName) ) {
        t.setName(" renamed to: " + newName + " from:" + reallyName)
      }
      p
    }
    finally {
      val t = Thread.currentThread()
      if ( !reallyName.equals(t.getName) ) {
        // TODO println has to be deleted !!!
        System.err.println(System.currentTimeMillis +  " after will change back from " + t.getName + " to " + reallyName)
        t.setName(reallyName)
      }
      // TODO println has to be deleted !!! 
      System.err.println("the thread name is: " + Thread.currentThread().getName)
    }
*/
    p
  }

}


