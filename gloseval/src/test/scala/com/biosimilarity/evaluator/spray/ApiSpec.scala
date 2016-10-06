package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.Uri
import spray.io.ClientSSLEngineProvider

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

abstract class ApiTests(val apiUri: Uri, sslEngineProvider: ClientSSLEngineProvider)
    extends WordSpec
    with ApiClient
    with Matchers
    with BeforeAndAfterEach
    with ScalaFutures {

  val system: ActorSystem = ActorSystem()

  implicit val timeout: Timeout = Timeout(FiniteDuration(15, SECONDS))
  implicit val ec               = system.dispatcher

  val logger: Logger

  "The Api" should {

    "respond to a versionInfoRequest with a versionInfoResponse" in {
      val eventualResponse: Future[Response] =
        for {
          uri <- Future(apiUri)
          hc  <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          uri <- Future("/api")
          r   <- post(hc, uri, VersionInfoRequest)
          x   <- Future(read[Response](r.entity.asString))
          _   <- hc.ask(Http.CloseAll)
        } yield x
      whenReady(eventualResponse) { (response: Response) =>
        response.msgType shouldBe "versionInfoResponse"
        response.extractResponseContent match {
          case VersionInfoResponse(gv, sv, mdbv, rmqv) =>
            gv should equal(BuildInfo.version)
            sv should equal(BuildInfo.scalaVersion)
          case _ =>
            fail("should not happen")
        }
      }
    }

    "allow the administrator to create a session" in {
      val proc: Future[SessionUri] = for {
        uri <- Future(apiUri)
        hc  <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        isr <- openAdminSession(hc, uri, "admin@localhost", "a")
        _   <- hc.ask(Http.CloseAll)
      } yield isr.sessionURI
      proc.futureValue shouldNot be("")
    }

    "allow the administrator to query an empty database without crashing" in {
      val proc: Future[(JArray)] = for {
        uri      <- Future(apiUri)
        hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        isr      <- openAdminSession(hc, uri, "admin@localhost", "a")
        cnxn     <- makeQueryOnSelf(hc, uri, isr.sessionURI, "each([MESSAGEPOSTLABEL])")
        spwnssnA <- spawnSession(hc, uri, isr.sessionURI)
        a        <- sessionPing(hc, uri, spwnssnA)
        _        <- hc.ask(Http.CloseAll)
      } yield a
      proc.futureValue.values.length shouldBe 1
    }

    "allow the administrator to make connections" in {
      val proc: Future[JArray] = for {
        uri   <- Future(apiUri)
        hc    <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        isr   <- openAdminSession(hc, uri, "admin@localhost", "a")
        alice <- createSRPUser(hc, "alice@testing.com", "alice", "a")
        bob   <- createSRPUser(hc, "bob@testing.com", "bob", "b")
        _     <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
        // get alice's connections
        isrA     <- openSRPSession(hc, uri, "alice@testing.com", "a")
        spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
        _        <- getConnectionProfiles(hc, uri, spwnssnA)
        a        <- sessionPing(hc, uri, spwnssnA)
        _        <- hc.ask(Http.CloseAll)
      } yield a
      whenReady(proc, timeout(Span(60, Seconds))) {
        case (ja: JArray) =>
          logger.info(s"Alice's connections: ${pretty(render(ja))}")
          ja.values.length shouldBe 2
      }
    }

    "establish the correct number of connections" in {
      val proc: Future[(JArray, JArray, JArray)] = for {
        uri   <- Future(apiUri)
        hc    <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        isr   <- openAdminSession(hc, uri, "admin@localhost", "a")
        alice <- createSRPUser(hc, "alice@test.com", "alice", "a")
        bob   <- createSRPUser(hc, "bob@test.com", "bob", "b")
        carol <- createSRPUser(hc, "carol@test.com", "carol", "c")
        // make the connections
        _ <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
        _ <- makeConnection(hc, uri, isr.sessionURI, alice, carol, "alice_carol")
        // get alice's connections
        isrA     <- openSRPSession(hc, uri, "alice@test.com", "a")
        spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
        _        <- getConnectionProfiles(hc, uri, spwnssnA)
        a        <- sessionPing(hc, uri, spwnssnA)
        // get bob's connections
        isrB     <- openSRPSession(hc, uri, "bob@test.com", "b")
        spwnssnB <- spawnSession(hc, uri, isrB.sessionURI)
        _        <- getConnectionProfiles(hc, uri, spwnssnB)
        b        <- sessionPing(hc, uri, spwnssnB)
        // get carols's connections
        isrC     <- openSRPSession(hc, uri, "carol@test.com", "c")
        spwnssnC <- spawnSession(hc, uri, isrC.sessionURI)
        _        <- getConnectionProfiles(hc, uri, spwnssnC)
        c        <- sessionPing(hc, uri, spwnssnC)
        _        <- hc.ask(Http.CloseAll)
      } yield (a, b, c)

      whenReady(proc, timeout(Span(60, Seconds))) {
        case (ja: JArray, jb: JArray, jc: JArray) =>
          logger.info(s"Alice's connections: ${pretty(render(ja))}")
          logger.info(s"Bob's connections: ${pretty(render(jb))}")
          logger.info(s"Carol's connections: ${pretty(render(jc))}")
          ja.values.length shouldBe 3
          jb.values.length shouldBe 2
          jc.values.length shouldBe 2
      }
    }

    "return evalSubscribeResponse when querying using 'any', 'each' or 'all'" in {
      val proc: Future[(JArray)] = for {
        uri      <- Future(apiUri)
        hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
        ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
        _        <- makeQueryOnSelf(hc, uri, ssn, "each([MESSAGEPOSTLABEL])")
        _        <- makeQueryOnSelf(hc, uri, ssn, "any([MESSAGEPOSTLABEL])")
        _        <- makeQueryOnSelf(hc, uri, ssn, "all([MESSAGEPOSTLABEL])")
        a        <- sessionPing(hc, uri, ssn)
        _        <- hc.ask(Http.CloseAll)
      } yield a
      whenReady(proc) {
        case (ja: JArray) =>
          ja.arr.length shouldBe 1
          val rsp     = ja.arr.head.asInstanceOf[JObject]
          val msgType = (rsp \ "msgType").extract[String]
          msgType shouldBe "evalSubscribeResponse"
        case _ => fail("should not happen")
      }
    }

    "return evalSubscribeError when querying not using 'any', 'each' or 'all'" in {
      val proc: Future[(JArray)] = for {
        uri      <- Future(apiUri)
        hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
        ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
        _        <- makeQueryOnSelf(hc, uri, ssn, "lordfarquad([MESSAGEPOSTLABEL])")
        a        <- pingUntilPong(hc, uri, ssn)
        _        <- hc.ask(Http.CloseAll)
      } yield a
      whenReady(proc) {
        case (ja: JArray) =>
          ja.arr.length shouldBe 1
          val rsp     = ja.arr.head.asInstanceOf[JObject]
          val msgType = (rsp \ "msgType").extract[String]
          msgType shouldBe "evalSubscribeError"
        case _ => fail("should not happen")
      }
    }
  }

  "The Session Cam" should {
    "work" in {
      val proc: Future[String] = for {
        uri   <- Future(apiUri)
        hc    <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        alice <- createSRPUser(hc, "alice@test.com", "alice", "a")
        isrA  <- openSRPSession(hc, uri, "alice@test.com", "a")
        _     <- startCam(hc, uri, isrA.sessionURI)
        _     <- pingUntilPong(hc, uri, isrA.sessionURI)
        s     <- stopCam(hc, uri, isrA.sessionURI)
        _     <- hc.ask(Http.CloseAll)
      } yield s
      whenReady(proc) {
        case (s: String) =>
          println(s)
          s shouldNot be("")
        case _ => fail("should not happen")
      }
    }
  }
}

class ApiSpec extends ApiTests(Uri("https://localhost:9876/api"), clientSSLEngineProvider) with IntegrationPatience {

  val logger: Logger = LoggerFactory.getLogger(classOf[ApiSpec])

  var serverInstance: Option[Server] = None

  override def beforeEach(): Unit = {
    resetMongo()
    serverInstance = Some(Server().start())
    Thread.sleep(2000L)
    logger.info("finished waiting")
  }

  override def afterEach(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }
}
