package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.util.mongo.MongoQuery
import org.json4s.JsonAST.JObject
import org.json4s.{BuildInfo => _, _}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class DataIntegrityTest extends WordSpec with ApiClient with Matchers with BeforeAndAfterEach with ScalaFutures {

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

  "The Api" should {

    "make multiple users without corrupting database" in {
      val ablob = JObject(
        ("name", JString("Alice"))
          :: ("imgSrc" -> JString("data:image/pnglbase64,iVBORw0KGgoAAAANSUAAAElFTkSuQmCC"))
            :: Nil)
      val bblob = JObject(
        ("name", JString("Bob"))
          :: ("imgSrc" -> JString("data:image/pnglbase64,iVBORw0KGgoAAAANSUAAAElFTkSuQmCC"))
            :: Nil)
      val cblob = JObject(
        ("name", JString("Carol"))
          :: ("imgSrc" -> JString("data:image/pnglbase64,iVBORw0KGgoAAAANSUAAAElFTkSuQmCC"))
            :: Nil)
      val eventually: Future[Unit] =
        for {
          uri   <- Future(apiUri)
          hc    <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          alice <- createUser(hc, uri, "alice@testing.com", "a", ablob)
          bob   <- createUser(hc, uri, "bob@testing.com", "b", bblob)
          carol <- createUser(hc, uri, "carol@testing.com", "c", cblob)
        } yield ()

      whenReady(eventually) { (_) =>
        val qry = new MongoQuery()
        qry.printAliasCnxns()
        val allcnxns = qry.readAllAliasCnxns()
        allcnxns.toList.length shouldBe 4
        val invalids = qry.invalidAliasCnxns()
        invalids.length shouldBe 0
      }
    }
  }
}
