package com.protegra_ati.agentservices.core

import scala.concurrent.ops._
import com.protegra_ati.agentservices.core.util.ThreadRenamer._
import scala.util.Random

/* User: mgevantmakher
*/

object RenameThreadTest
{
  def main(arg: Array[ String ]) =
  {
    System.err.println("before everything")
    rename {
      System.err.println("Running thread has a name:" + Thread.currentThread().getName)
      val anonFunction = 1 + 1
      System.err.println("some addition in anonymous function: 1+1=" + anonFunction)
      System.err.println("some anonymous function")
    }("newName")



    for ( i <- 0 until 100 ) {
      spawn {
        rename {
          val in = 1 + 10 + Random.nextString(5)
          System.err.println("function made some things=" + in)
        }("I know who you are " + i)
      }

    }

  }

}
