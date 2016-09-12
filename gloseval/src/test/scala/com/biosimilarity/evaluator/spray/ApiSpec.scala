package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.spray.util._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import spray.can.Http

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ApiSpec extends WordSpec with ApiClient with Matchers with BeforeAndAfterEach with ScalaFutures with IntegrationPatience {

  val system: ActorSystem = ActorSystem()

  implicit val timeout: Timeout = Timeout(FiniteDuration(15, SECONDS))
  implicit val ec               = system.dispatcher

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

  "A versionInfoRequest" should {
    "result in a versionInfoResponse" in {
      val eventualResponse: Future[AltResponse[VersionInfoResponse]] =
        for {
          hc <- eventualHostConnector(system)
          r  <- post(hc, VersionInfoRequest)
          x  <- Future(read[AltResponse[VersionInfoResponse]](r.entity.asString))
          _  <- hc.ask(Http.CloseAll)
        } yield x
      whenReady(eventualResponse) { (msg: AltResponse[VersionInfoResponse]) =>
        msg.msgType shouldBe "versionInfoResponse"
        msg.content.glosevalVersion should equal(BuildInfo.version)
        msg.content.scalaVersion should equal(BuildInfo.scalaVersion)
        msg.content.mongoDBVersion should equal(mongoVersion().getOrElse("n/a"))
        msg.content.rabbitMQVersion should equal(rabbitMQVersion().getOrElse("n/a"))
      }
    }
  }

  "The Administrator" should {
    "be able to create a session" in {
      val proc: Future[SessionUri] = for {
        hc <- eventualHostConnector(system)
        s  <- openAdminSession(hc)
        _  <- hc.ask(Http.CloseAll)
      } yield s
      proc.futureValue shouldNot be("")
    }

    "query empty database without crashing" in {
      val proc: Future[(JArray)] = for {
        hc       <- eventualHostConnector(system)
        ssn      <- openAdminSession(hc)
        cnxn     <- makeQueryOnSelf(hc, ssn, "each([MESSAGEPOSTLABEL])")
        spwnssnA <- spawnSession(hc, ssn)
        a        <- sessionPing(hc, spwnssnA)
        _        <- hc.ask(Http.CloseAll)
      } yield a
      proc.futureValue.values.length shouldBe 1
    }

    "be able to make connections" in {
      val proc: Future[JArray] = for {
        hc    <- eventualHostConnector(system)
        ssn   <- openAdminSession(hc)
        alice <- createSRPUser(hc, "alice@testing.com", "alice", "a")
        bob   <- createSRPUser(hc, "bob@testing.com", "bob", "b")
        _     <- makeConnection(hc, ssn, alice, bob, "alice_bob")
        // get alice's connections
        ssnA     <- openSRPSession(hc, "alice@testing.com", "a")
        spwnssnA <- spawnSession(hc, ssnA)
        _        <- getConnectionProfiles(hc, spwnssnA)
        a        <- sessionPing(hc, spwnssnA)
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
        hc    <- eventualHostConnector(system)
        ssn   <- openAdminSession(hc)
        alice <- createSRPUser(hc, "alice@test.com", "alice", "a")
        bob   <- createSRPUser(hc, "bob@test.com", "bob", "b")
        carol <- createSRPUser(hc, "carol@test.com", "carol", "c")
        // make the connections
        _ <- makeConnection(hc, ssn, alice, bob, "alice_bob")
        _ <- makeConnection(hc, ssn, alice, carol, "alice_carol")
        // get alice's connections
        ssnA     <- openSRPSession(hc, "alice@test.com", "a")
        spwnssnA <- spawnSession(hc, ssnA)
        _        <- getConnectionProfiles(hc, spwnssnA)
        a        <- sessionPing(hc, spwnssnA)
        // get bob's connections
        ssnB     <- openSRPSession(hc, "bob@test.com", "b")
        spwnssnB <- spawnSession(hc, ssnB)
        _        <- getConnectionProfiles(hc, spwnssnB)
        b        <- sessionPing(hc, spwnssnB)
        // get carols's connections
        ssnC     <- openSRPSession(hc, "carol@test.com", "c")
        spwnssnC <- spawnSession(hc, ssnC)
        _        <- getConnectionProfiles(hc, spwnssnC)
        c        <- sessionPing(hc, spwnssnC)
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

    "return evalSubscribeResponse when querying using any, each or all" in {
      val proc: Future[(JArray)] = for {
        hc   <- eventualHostConnector(system)
        tssn <- openAdminSession(hc)
        ssn  <- spawnSession(hc, tssn)
        _ <- {
          // uncomment the following line allow the test to run faster
          //for (actor <- SessionManager.getSession(ssn) ) actor ! SetPongTimeout(1.seconds)
          makeQueryOnSelf(hc, ssn, "each([MESSAGEPOSTLABEL])")
          makeQueryOnSelf(hc, ssn, "any([MESSAGEPOSTLABEL])")
          makeQueryOnSelf(hc, ssn, "all([MESSAGEPOSTLABEL])")
        }
        a <- sessionPing(hc, ssn)
        _ <- hc.ask(Http.CloseAll)
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

    "return evalSubscribeError when querying not using any, each or all" in {
      val proc: Future[(JArray)] = for {
        hc   <- eventualHostConnector(system)
        tssn <- openAdminSession(hc)
        ssn  <- spawnSession(hc, tssn)
        _ <- {
          // uncomment the following line to allow the test to run faster
          //for (actor <- SessionManager.getSession(ssn) ) actor ! SetPongTimeout(1.seconds)
          makeQueryOnSelf(hc, ssn, "lordfarquad([MESSAGEPOSTLABEL])")
        }
        //a <- sessionPing(ssn)
        a <- pingUntilPong(hc, ssn)
        _ <- hc.ask(Http.CloseAll)
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

  "The session cam" should {
    "work" in {
      val proc: Future[String] = for {
        hc    <- eventualHostConnector(system)
        alice <- createSRPUser(hc, "alice@test.com", "alice", "a")
        ssnA  <- openSRPSession(hc, "alice@test.com", "a")
        _     <- startCam(hc, ssnA)
        _     <- pingUntilPong(hc, ssnA)
        s     <- stopCam(hc, ssnA)
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
