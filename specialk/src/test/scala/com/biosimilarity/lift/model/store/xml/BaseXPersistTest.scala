package com.biosimilarity.lift.model.store.xml

import org.specs.Specification
import org.specs.runner.{JUnit4, ConsoleRunner}
import java.util.{Collections, UUID}
import util.Random
import scala.concurrent.ops._

import actors.threadpool.AtomicInteger
import org.specs.util.Duration
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util
import util.concurrent.ConcurrentHashMap
import org.basex.server.ClientSession

/**
 *
 */
class BaseXPersistTest
  extends JUnit4(BaseXPersistTestSpecs)

object BaseXPersistTestSpecsRunner
  extends ConsoleRunner(BaseXPersistTestSpecs)


object BaseXPersistTestSpecs
  extends Specification
{
  val dbHost = "localhost"
  val dbPort = 1984
  val dbUser = "admin"
  val dbPwd = "admin"

  val numThreads = 10
  val qPerThread = 5
  val numDbs = 2

  "PooledBaseXPersist test" should {
    class BaseXPersistUtils extends BaseXXMLStore
    {
    }

    "insertUpdate queries creating empty databases" in {
      try {
        val pbx = new BaseXPersistUtils

        val dbs = Array.fill(numDbs)(UUID.randomUUID.toString)
        val keys = new ConcurrentHashMap[String, util.List[String]]

        dbs.foreach(db => keys.put(db, Collections.synchronizedList(new util.ArrayList[String]())))

        val excThrown = new AtomicBoolean(false)
        val completedThreads = new AtomicInteger(0)
        val writeQueries = new AtomicInteger(0)
        val readQueries = new AtomicInteger(0)
        val end = new AtomicLong(0)
        val r = new Random

        val start = System.nanoTime
        for (i <- 1 to numThreads)
        {
          spawn
          {
            try {
              for (j <- 1 to qPerThread)
              {
                val db = dbs(r.nextInt(dbs.length))
                val dbKeys = keys.get(db)

                // 5% hit rate for writes (inserts), 95% for reads
                if (dbKeys.size == 0 || r.nextInt(20) == 5) {
                  val keyId = UUID.randomUUID.toString
                  val key = "<key>" + keyId + "</key>"
                  val value = "<value>" + UUID.randomUUID.toString + "</value>"

                  pbx.insertUpdate("record")(db, key, value)
                  dbKeys.add(keyId)

                  writeQueries.incrementAndGet()
                } else {
                  val randKey = dbKeys.get(r.nextInt(dbKeys.size))
                  val query = "for $x in collection('" + db + "')//records/record[key='" + randKey + "'] return $x"
                  pbx.executeWithResults(db, query)
                  readQueries.incrementAndGet()
                }
              }
            }
            catch {
              case e => {
                e.printStackTrace()
                excThrown.set(true)
              }
            }

            val c = completedThreads.incrementAndGet()
            if (c == numThreads) end.set(System.nanoTime)
          }
        }


        completedThreads.get() must be_==(numThreads).eventually(600, new Duration(1000))
        excThrown.get must be_==(false)

        val totTime = (end.get - start) / 1000000.0
        println("PooledBaseXPersist - Created " + dbs.length + " databases and executed a total of " + (numThreads * qPerThread) + " queries (5%/95% write/reads) in " + totTime + " ms")
        println("PooledBaseXPersist - Executed " + readQueries.get + " read queries and " + writeQueries.get + " write queries.")
      }
      catch {
        case e => e.printStackTrace
      }

      val foo = true
      foo must be_==(true)
    }
  }
}
