package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.time.{Second, Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.Uri
import spray.io.ClientSSLEngineProvider

import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.util.mongo.MongoQuery

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

abstract class ApiTests(val apiUri: Uri, sslEngineProvider: ClientSSLEngineProvider)
    extends WordSpec
    with ApiClient
    with Matchers
    with BeforeAndAfterEach
    with ScalaFutures {

  val system: ActorSystem = ActorSystem()

  implicit val ec = system.dispatcher

  implicit val timeout: Timeout

  val logger: Logger

  "The Api" should {

    "respond to a versionInfoRequest with a versionInfoResponse" in {

      val eventualResponse: Future[Response] =
        for {
          uri          <- Future(apiUri)
          hc           <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          uri          <- Future("/api")
          httpResponse <- httpPost(hc, uri, VersionInfoRequest)
          response     <- Future(read[Response](httpResponse.entity.asString))
          _            <- hc.ask(Http.CloseAll)
        } yield response

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

      val eventualSessionURI: Future[String] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          initializeSessionResponse <- openAdminSession(hc, uri, "admin@localhost", "a")
          _                         <- hc.ask(Http.CloseAll)
        } yield initializeSessionResponse.sessionURI

      eventualSessionURI.futureValue shouldNot be("")
    }

    "allow the administrator to query an empty database without crashing" in {

      val eventualJArray: Future[JArray] =
        for {
          uri        <- Future(apiUri)
          hc         <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          isr        <- openAdminSession(hc, uri, "admin@localhost", "a")
          _          <- makeQueryOnSelf(hc, uri, isr.sessionURI, "each([MESSAGEPOSTLABEL])")
          sessionUri <- spawnSession(hc, uri, isr.sessionURI)
          jArray     <- sessionPing(hc, uri, sessionUri)
          _          <- hc.ask(Http.CloseAll)
        } yield jArray

      eventualJArray.futureValue.values.length shouldBe 1
    }

    "allow the administrator to make connections" in {

      val eventualJArray: Future[JArray] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          isr      <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice    <- createSRPUser(hc, "alice@testing.com", "alice", "a")
          bob      <- createSRPUser(hc, "bob@testing.com", "bob", "b")
          _        <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          isrA     <- openSRPSession(hc, uri, "alice@testing.com", "a")
          spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
          _        <- getConnectionProfiles(hc, uri, spwnssnA)
          jArray   <- pingUntilPong(hc, uri, spwnssnA)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (ja: JArray) =>
        println(s"Alice's connections: ${pretty(render(ja))}")
        ja.values.length shouldBe 3
      }
    }

    "allow the administrator to make connections (dup)" in {

      val eventualJArray: Future[JArray] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          isr      <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice    <- createSRPUser(hc, "alice@testing.com", "alice", "a")
          bob      <- createSRPUser(hc, "bob@testing.com", "bob", "b")
          _        <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          isrA     <- openSRPSession(hc, uri, "alice@testing.com", "a")
          spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
          _        <- getConnectionProfiles(hc, uri, spwnssnA)
          jArray   <- pingUntilPong(hc, uri, spwnssnA)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (ja: JArray) =>
        println(s"Alice's connections: ${pretty(render(ja))}")
        ja.values.length shouldBe 3
      }
    }

    "establish the correct number of connections" in {

      val eventualTuple: Future[(JArray, JArray, JArray)] =
        for {
          uri         <- Future(apiUri)
          hc          <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          isr         <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice       <- createSRPUser(hc, "alice@test.com", "alice", "a")
          bob         <- createSRPUser(hc, "bob@test.com", "bob", "b")
          carol       <- createSRPUser(hc, "carol@test.com", "carol", "c")
          _           <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          _           <- makeConnection(hc, uri, isr.sessionURI, alice, carol, "alice_carol")
          isrA        <- openSRPSession(hc, uri, "alice@test.com", "a")
          spwnssnA    <- spawnSession(hc, uri, isrA.sessionURI)
          _           <- getConnectionProfiles(hc, uri, spwnssnA)
          jArrayAlice <- pingUntilPong(hc, uri, spwnssnA)
          isrB        <- openSRPSession(hc, uri, "bob@test.com", "b")
          spwnssnB    <- spawnSession(hc, uri, isrB.sessionURI)
          _           <- getConnectionProfiles(hc, uri, spwnssnB)
          jArrayBob   <- pingUntilPong(hc, uri, spwnssnB)
          isrC        <- openSRPSession(hc, uri, "carol@test.com", "c")
          spwnssnC    <- spawnSession(hc, uri, isrC.sessionURI)
          _           <- getConnectionProfiles(hc, uri, spwnssnC)
          jArrayCarol <- pingUntilPong(hc, uri, spwnssnC)
          _           <- hc.ask(Http.CloseAll)
        } yield (jArrayAlice, jArrayBob, jArrayCarol)

      whenReady(eventualTuple) {
        case (ja: JArray, jb: JArray, jc: JArray) =>
          println(s"Alice's connections: ${pretty(render(ja))}")
          ja.values.length shouldBe 4
          println(s"Bob's connections: ${pretty(render(jb))}")
          jb.values.length shouldBe 3
          println(s"Carol's connections: ${pretty(render(jc))}")
          jc.values.length shouldBe 3
      }
    }

    "return evalSubscribeResponse when querying using 'any', 'each' or 'all'" ignore {

      val eventualJArray: Future[JArray] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
          _        <- makeQueryOnSelf(hc, uri, ssn, "each([MESSAGEPOSTLABEL])")
          _        <- makeQueryOnSelf(hc, uri, ssn, "any([MESSAGEPOSTLABEL])")
          _        <- makeQueryOnSelf(hc, uri, ssn, "all([MESSAGEPOSTLABEL])")
          jArray   <- sessionPing(hc, uri, ssn)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (ja: JArray) =>
        val rsp     = ja.arr.head.asInstanceOf[JObject]
        val msgType = (rsp \ "msgType").extract[String]
        ja.arr.length shouldBe 1
        msgType shouldBe "evalSubscribeResponse"
      }
    }

    "return evalSubscribeError when querying not using 'any', 'each' or 'all'" in {

      val eventualJArray: Future[(JArray)] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
          _        <- makeQueryOnSelf(hc, uri, ssn, "lordfarquad([MESSAGEPOSTLABEL])")
          jArray   <- pingUntilPong(hc, uri, ssn)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        val rsp     = jArray.arr.head.asInstanceOf[JObject]
        val msgType = (rsp \ "msgType").extract[String]
        jArray.arr.length shouldBe 2
        msgType shouldBe "evalSubscribeError"
      }
    }
  }

  "The Session Cam" should {

    "work" in {

      val eventualString: Future[String] = for {
        uri   <- Future(apiUri)
        hc    <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
        alice <- createSRPUser(hc, "alice@test.com", "alice", "a")
        isrA  <- openSRPSession(hc, uri, "alice@test.com", "a")
        _     <- startCam(hc, uri, isrA.sessionURI)
        _     <- pingUntilPong(hc, uri, isrA.sessionURI)
        s     <- stopCam(hc, uri, isrA.sessionURI)
        _     <- hc.ask(Http.CloseAll)
      } yield s

      whenReady(eventualString) { (s: String) =>
        println(s)
        s shouldNot be("")
      }
    }
  }

  "The Importer" should {

    "import the 'singlePost' test file " in {

      val rslt = Importer.fromTest("singlePost")
      rslt shouldBe 0
      val qry = new MongoQuery()
      qry.printAliasCnxns()
      val conts =  qry.readAllAliasCnxns()
      conts("Alice").biCnxnBouncers.length shouldBe 1
      conts("Bob").biCnxnBouncers.length shouldBe 1
      // Need to create an SOC to get the orphans issue fixed
      //conts("Alice").orphans.length shouldBe 0
      //conts("Bob").orphans.length shouldBe 0

    }

    "import the 'zeroToTen' test file " in {
      val rslt = Importer.fromTest("zeroToTen")
      rslt shouldBe 0
      val qry = new MongoQuery()
      qry.printAliasCnxns()
      val conts =  qry.readAllAliasCnxns()
      conts.size shouldBe 12
      conts.foreach( pr => {
        val cnxn = pr._2
        cnxn.biCnxnBouncers.length shouldBe 1
        //cnxn.orphans.length shouldBe 1
      })
      conts(" Lucky Seven").cnxns.length shouldBe 3
      conts(" Zero").cnxns.length shouldBe 11
      conts("NodeAdmin QueenSplicious").cnxns.length shouldBe 11
    }

  }

}

class ApiSpec extends ApiTests(Uri("https://localhost:9876/api"), clientSSLEngineProvider) with IntegrationPatience {

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
}
