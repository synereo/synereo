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
    println("before everything")
    rename {
      println("Running thread has a name:" + Thread.currentThread().getName)
      val anonFunction = 1 + 1
      println("some addition in anonymous function: 1+1=" + anonFunction)
      println("some anonymous function")
    }("newName")



    for ( i <- 0 until 100 ) {
      spawn {
        rename {
          val in = 1 + 10 + Random.nextString(5)
          println("function made some things=" + in)
        }("I know who you are " + i)
      }

    }

  }

}
