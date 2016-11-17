package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.util.mongo.MongoQuery
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ImporterTest extends WordSpec with ApiClient with Matchers with BeforeAndAfterEach with ScalaFutures{

  val system: ActorSystem = ActorSystem()

  implicit val ec = system.dispatcher

  val logger: Logger = LoggerFactory.getLogger(classOf[ApiSpec])

  val maxNumberOfPingUntilPongs = 5

  val timeoutLength: Int = EvalConfigWrapper.readIntOrElse("pongTimeout", 15) * (maxNumberOfPingUntilPongs + 1)

  override implicit val patienceConfig = PatienceConfig(timeout = Span(timeoutLength, Seconds))

  implicit val timeout: Timeout = Timeout(FiniteDuration(timeoutLength, SECONDS))

  var serverInstance: Option[Server] = None

  override def beforeEach(): Unit = {
    resetMongo()
    serverInstance = Some(Server().start())
    Thread.sleep(10000L)
    logger.info("finished waiting")
  }

  override def afterEach(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }

  val apiUri = Uri("https://localhost:9876/api")

  val sslEngineProvider = clientSSLEngineProvider

  "The Importer" should {

    "import the 'zeroToTen' test file " ignore {
      val rslt = Importer.fromTestData("zeroToTen")
      rslt shouldBe 0
      val qry = new MongoQuery()
      qry.printAliasCnxns()
      val conts = qry.readAllAliasCnxns()
      conts.size shouldBe 12
      conts.foreach(pr => {
        val cnxn = pr._2
        cnxn.biCnxnBouncers.length shouldBe 1
        //cnxn.orphans.length shouldBe 1
      })
      conts(" Lucky Seven").cnxns.length shouldBe 3
      conts(" Zero").cnxns.length shouldBe 11
      conts("NodeAdmin QueenSplicious").cnxns.length shouldBe 11
    }

    "import the 'singlePost' test file " ignore {

      val rslt = Importer.fromTestData("singlePost")
      rslt shouldBe 0
      val qry = new MongoQuery()
      qry.printAliasCnxns()
      val conts = qry.readAllAliasCnxns()
      conts("Alice").biCnxnBouncers.length shouldBe 1
      conts("Bob").biCnxnBouncers.length shouldBe 1
      // TODO: Create an SOC to get the orphans issue fixed
      // conts("Alice").orphans.length shouldBe 0
      // conts("Bob").orphans.length shouldBe 0
    }
  }
}
