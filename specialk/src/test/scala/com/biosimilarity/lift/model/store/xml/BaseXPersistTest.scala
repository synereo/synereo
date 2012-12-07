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
  /*val dbsToCreate = 1000
  val hitRate = 0.05
  val records = "<records></records>"
  val simThreads = 100*/

  /*"Concurrent ClientSession" should {
    "execute queries concurrently" in {
      skip("")
      val threads = 100
      val dbs = Array.fill(25)(UUID.randomUUID.toString)
      val csarr = Array.fill(threads)(new ClientSession("localhost", 1984, "admin", "admin"))

      val complThreads = new AtomicInteger(0)

      val r = new Random
      val insertTemplate:String = "insert node %NODE% into for $db in collection('%COLLNAME%')/records return $db"

      val qPerThread = 100

      val start = System.nanoTime
      val end = new AtomicLong(0)

      val keys = new ConcurrentHashMap[String, util.List[String]]

      for (i <- 0 to threads-1)
      {
        spawn
        {
          Thread.sleep(r.nextInt(1000))

          val cs : ClientSession = csarr(i)

          try {

            for (j <- 1 to qPerThread) {
              Thread.sleep(r.nextInt(50))

              val db = dbs(r.nextInt(dbs.length))

              // Ensure the DB is created
              cs.execute(new Check(db))
              val result = cs.info

              val dbKeys = keys.get(db)
              // 10% chance of hitting an insert
              if (dbKeys.size == 0 || r.nextInt(10) == 5) {
                // Insert a random record into the
                val key = UUID.randomUUID.toString
                dbKeys.add(key)
                val node = "<records><record><key>" + key + "</key><value>" + UUID.randomUUID + "</value></record></records>"
                val iq = insertTemplate.replace("%NODE%", node).replace("%COLLNAME%", db)
                cs.execute(new XQuery(iq))
              } else {
                val randKey = dbKeys.get(r.nextInt(dbKeys.size))
                val query = "//records/record[key=\"" + randKey + "\"]"
                val str = cs.execute(new XQuery(query))
              }
            }
          }
          catch {
            case e => e.printStackTrace
          }
          finally {
            cs.close
          }

          val cur = complThreads.incrementAndGet()
          if (cur == threads) end.set(System.nanoTime)
        }
      }

      complThreads.get must be_==(threads).eventually(300, new Duration(1000))

      val total = (end.get - start) / 1000000.0
      println("Ran " + complThreads.get + " threads in " + total + " ms")
    }

  }*/

  val numThreads = 200
  val qPerThread = 500
  val numDbs = 100

  "Apache Commons Pool test" should {

    "create and access many objects" in {
      skip("")
      val poolSize = 1000
      val r = new Random
      val pool = new BaseXSessionPool("localhost", 1984, "admin", "admin")

      val s1 = System.nanoTime()
      val sessions = Array.fill[ClientSession](poolSize)(pool.borrowClientSession)
      val t1 = (System.nanoTime - s1)/1000000.0
      println("Took " + t1 + " ms to get " + poolSize + " ClientSession objects from pool (new objects created, single threaded)")

      val s2 = System.nanoTime()
      sessions.foreach(cs => pool.returnClientSession(cs))
      val t2 = (System.nanoTime - s2)/1000000.0
      println("Took " + t2 + " ms to return " + poolSize + " ClientSession objects to the pool (single threaded)")

      val dbs = Array.fill(numDbs)(UUID.randomUUID.toString)
      val keys = new ConcurrentHashMap[String, util.List[String]]

      dbs.foreach(db => keys.put(db, Collections.synchronizedList(new util.ArrayList[String]())))



      /*val s3 = System.nanoTime()
      val sb2 = Array.fill[StringBuffer](poolSize)(StringBufferPool.getStringBuffer)
      sb2.foreach(s => StringBufferPool.dropStringBuffer(s))
      val t3 = (System.nanoTime - s3)/1000000.0
      println("Took " + t3 + " ms to take and return " + poolSize + " StringBuffer objects to the pool (objects existed, single threaded)")

      val a4 = new AtomicInteger(0)
      val s4 = System.nanoTime()
      val e4 = new AtomicLong(0)
      for (i <- 1 to poolSize) {
        spawn {
          if (a4.incrementAndGet() == poolSize) e4.set(System.nanoTime)
        }
      }

      a4.get() must be_==(poolSize).eventually(10, new Duration(1000))
      val t4 = (e4.get - s4)/1000000.0
      println("Took " + t4 + " ms to spawn " + poolSize + " threads with no work")

      val a5 = new AtomicInteger(0)
      val s5 = System.nanoTime()
      val e5 = new AtomicLong(0)
      for (i <- 1 to poolSize) {
        spawn {
          if (a5.incrementAndGet() == poolSize) e5.set(System.nanoTime)
        }
      }

      a5.get() must be_==(poolSize).eventually(10, new Duration(1000))
      val t5 = (e5.get - s5)/1000000.0
      println("Took " + t5 + " ms to spawn " + poolSize + " threads with no work")


      val a6 = new AtomicInteger(0)
      val s6 = System.nanoTime()
      val e6 = new AtomicLong(0)
      for (i <- 1 to poolSize) {
        spawn {
          try {
            val sb = StringBufferPool.getStringBuffer
            StringBufferPool.dropStringBuffer(sb)
          } catch {
            case e => e.printStackTrace
          }

          if (a6.incrementAndGet() == poolSize) e6.set(System.nanoTime)
        }
      }

      a6.get() must be_==(poolSize).eventually(10, new Duration(1000))
      val t6 = (e6.get - s6)/1000000.0
      println("Took " + t6 + " ms to spawn " + poolSize + " threads and get/return 1000 objects to the pool")


      val a7 = new AtomicInteger(0)
      val s7 = System.nanoTime()
      val e7 = new AtomicLong(0)
      for (i <- 1 to poolSize) {
        spawn {
          try {
            Thread.sleep(r.nextInt(25))
            val sb = StringBufferPool.getStringBuffer
            val id = sb.toString
            println("B i: " + i + " - " + id)
            StringBufferPool.dropStringBuffer(sb)
            println("R i: " + i + " - " + id)
          } catch {
            case e => e.printStackTrace
          }

          if (a7.incrementAndGet() == poolSize) e7.set(System.nanoTime)
        }
      }

      a7.get() must be_==(poolSize).eventually(10, new Duration(1000))
      val t7 = (e7.get - s7)/1000000.0
      println("Took " + t7 + " ms to spawn " + poolSize + " threads and get/return 1000 objects to the pool")*/

      true must be_==(true)
    }
  }

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




  /*"BaseXXMLStore test" should {
    "foobar" in {
      val cs = new ClientSession("localhost", 1984, "admin", "admin")
      try {
        val dbName = "testDb"
        val res = cs.execute(new Check(dbName))
        val res2 = cs.execute(new Check(dbName))

        println("res: " + res)
        println("res2: " + res2)
      }
      catch {
        case e => {
          e.printStackTrace
        }
      }
    }

    "create all new databases and execute queries in a threaded manner" in {
      skip("")
      val start = System.nanoTime
      val excThrown = new AtomicBoolean(false)
      val bx = new BaseXPersistUtils
      val numCreated = new AtomicInteger(0)
      val c = dbsToCreate / simThreads
      val end = new AtomicLong(0)
      val total = c*simThreads

      for (i <- 1 to simThreads) {
        spawn
        {
          try {
            for (j <- 1 to c)
            {
              val collectionName = UUID.randomUUID().toString
              bx.insertUpdate("record")(collectionName, UUID.randomUUID().toString, UUID.randomUUID().toString)
              val cur = numCreated.incrementAndGet()
              if (cur == total) end.set(System.nanoTime)
            }
          }
          catch {
            case e => {
              e.printStackTrace
              excThrown.set(true)
            }
          }
        }
      }

      numCreated.get() must be_==(total).eventually(120, new Duration(1000))
      excThrown.get must be_==(false)

      val totTime = (end.get - start) / 1000000.0
      println("Created " + dbsToCreate + " databases in " + totTime + " ms using CreateAndOpen (single connection) in " + simThreads + " separate threads")
    }

    "create 5% new databases and execute queries in a threaded manner" in {
      skip("")
      val start = System.nanoTime
      val excThrown = new AtomicBoolean(false)
      val bx = new BaseXPersistUtils
      val numCreated = new AtomicInteger(0)
      val c = dbsToCreate / simThreads
      val end = new AtomicLong(0)
      val total = c*simThreads

      val numNewDbs = (dbsToCreate * 0.05).ceil.toInt
      val dbIds = Array.fill(numNewDbs)(UUID.randomUUID.toString)
      val r = new Random

      for (i <- 1 to simThreads) {
        spawn
        {
          try {
            for (j <- 1 to c)
            {
              val collectionName = dbIds(r.nextInt(numNewDbs))
              bx.insertUpdate("record")(collectionName, UUID.randomUUID().toString, UUID.randomUUID().toString)
              val cur = numCreated.incrementAndGet()
              if (cur == total) end.set(System.nanoTime)
            }
          }
          catch {
            case e => {
              e.printStackTrace
              excThrown.set(true)
            }
          }
        }
      }

      numCreated.get() must be_==(total).eventually(120, new Duration(1000))
      excThrown.get must be_==(false)

      val totTime = (end.get - start) / 1000000.0
      println("Created " + numNewDbs + " databases and executed a total of " + dbsToCreate + " queries in " + totTime + " ms using CreateAndOpen (single connection) in " + simThreads + " separate threads")
    }
  }

  def existsTemplate( recordType : String ) : String = {
    (
      "let $root := collection('%COLLNAME%')/records "
        + "let $key := %KEY% "
        + "for $rcrd in $root/%RECORDTYPE% "
        + "let $rcrdkey := $rcrd/[1] "
        + "where deep-equal($key, $rcrd/[1]) "
        + "return (exists($rcrd)) "
      ).replace( "%RECORDTYPE%", recordType )
  }

  def executeScalar(cs: ClientSession, collectionName: String, query: String): String =
  {

    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {

      cs.setOutputStream(srvrRspStrm)

      cs.execute(new XQuery(query))
      srvrRspStrm.toString
    }
    catch {
      case e: BaseXException => {
        //caught in code review, make more efficient
        throw e
      }
    }
    finally {
      // Reset output stream
      cs.execute(new Close())
      srvrRspStrm.close()
    }
  }

  def exists( cs:ClientSession, recordType : String )(collectionName: String, key: String): Boolean =
  {
    //val clientSession = clientSessionFromConfig
    val eTemplate : String = existsTemplate( recordType );

    val existsQry =
      eTemplate.replace(
        "%KEY%",
        key
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    try {
      val results = executeScalar(cs, collectionName, existsQry)
      results match {
        case "" => false
        case _ => true
      }
    }
    catch {
      case e: BaseXException => {
        //        println(e)
        false
      }
    }
  }

  def insertUpdate(cs: ClientSession) {

  }

  "BaseXConnectionPool test" should {
    val pool = BaseXConnectionPool
    val start = System.nanoTime
    val excThrown = new AtomicBoolean(false)
    val numCreated = new AtomicInteger(0)
    val c = dbsToCreate / simThreads
    val end = new AtomicLong(0)
    val total = c*simThreads

    val numNewDbs = (dbsToCreate * 0.05).ceil.toInt
    val dbIds = Array.fill(numNewDbs)(UUID.randomUUID.toString)
    val r = new Random

    for (i <- 1 to simThreads) {
      spawn
      {
        try {
          val cs = pool.getConnection()
          try {

          }
          finally {
            pool.dropConnection(cs)
          }
        }
        catch {
          case e => {
            e.printStackTrace
            excThrown.set(true)
          }
        }
      }
    }
  }*/
}
