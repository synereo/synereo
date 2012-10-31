package com.protegra_ati.agentservices.core.util

import scala.util.continuations._
import com.biosimilarity.lift.lib.moniker._
import com.protegra_ati.agentservices.core.util.ThreadRenamer._
import scala.util.Random
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

/* User: mgevantmakher
*/

object RenameThreadTest extends ThreadPoolRunners
{
  override def numWorkers = 50

  def main(arg: Array[ String ]) =
  {
    System.err.println("before everything")
    resetSpawnDemo()
    //    rename {
    //      System.err.println("Running thread has a name:" + Thread.currentThread().getName)
    //      val anonFunction = 1 + 1
    //      System.err.println("some addition in anonymous function: 1+1=" + anonFunction)
    //      System.err.println("some anonymous function")
    //    }("newName")
    //
    //
    //
    //    for ( i <- 0 until 100 ) {
    //      Thread.sleep(20000)
    //      spawn {
    //
    //        rename {
    //          val in = 1 + 10 + Random.nextString(5)
    //          Thread.sleep(25000)
    //          System.err.println("function made some things=" + in)
    //        }("I know who you are " + i)
    //
    //      }
    //
    //    }


    Thread.sleep(500000)
  }


  def resetSpawnDemo() =
  {

    System.err.println("reset shift running ...")
    for ( i <- 0 until 100 ) {

      Thread.sleep(5000)

      reset {
        System.err.println("in reset")
        spawn {
          rename {
            val t = Thread.currentThread()
            System.err.println("running on thread:" + t.getName + " " + t.getId)
            Thread.sleep(5000)
            System.err.println("just a test")
            // do something
          }("my renamed thread " + i)
        }
        shift {
          (cont: Unit => Unit) => {
            System.err.println("in shift")
          }
        }
        //for ( j <- 0 until 100 ) {

      }
    }
  }

  //}


}
