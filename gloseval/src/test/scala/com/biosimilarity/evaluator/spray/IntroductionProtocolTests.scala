package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.util.mongo.MongoQuery
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.Uri
import akka.pattern.ask

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn


import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class IntroductionProtocolTests extends WordSpec with ApiClient with Matchers with BeforeAndAfterEach with ScalaFutures{

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

  "The Administrator" should {

    "successfully initiate a connection" in {

      val eventualTuple: Future[(JArray, JArray)] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri <- Future(apiUri)
          hc <- ehc
          isr <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice <- createSRPUser(hc, uri, "alice@test.com", "alice", "a")
          bob <- createSRPUser(hc, uri, "bob@test.com", "bob", "b")
          _ <- getConnectionProfiles(hc, uri, isr.sessionURI)
          jArrayAdmin <- pingUntilPong(hc, uri, isr.sessionURI)
          cnxns <- extractConnections(jArrayAdmin)
          //  test cnxns?
          introRslt <- beginIntroduction(hc, uri, isr.sessionURI, cnxns(0), cnxns(1) )

          isrA <- openSRPSession(hc, uri, "alice@test.com", "a")
          spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
          jArrayAlice <- pingUntilPong(hc, uri, spwnssnA)
          isrB <- openSRPSession(hc, uri, "bob@test.com", "b")
          spwnssnB <- spawnSession(hc, uri, isrB.sessionURI)
          _ <- getConnectionProfiles(hc, uri, spwnssnB)
          jArrayBob <- pingUntilPong(hc, uri, spwnssnB)
          _ <- hc.ask(Http.CloseAll)
        } yield (jArrayAlice, jArrayBob)).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualTuple) {
        case (ja: JArray, jb: JArray) =>
          println(s"Alice's feed: ${pretty(render(ja))}")
          println(s"Bob's feed: ${pretty(render(jb))}")
      }
    }
  }
}
