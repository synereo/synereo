package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.JsonDSL._
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

  implicit val ec = system.dispatcher

  implicit val timeout: Timeout

  val logger: Logger

  "The Api" should {

    "respond to a versionInfoRequest with a versionInfoResponse" in {

      val eventualResponse: Future[Response] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri          <- Future(apiUri)
          hc           <- ehc
          httpResponse <- httpPost(hc, apiUri, VersionInfoRequest)
          response     <- Future(read[Response](httpResponse.entity.asString))
          _            <- hc.ask(Http.CloseAll)
        } yield response).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

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

      val eventualSessionURI: Future[String] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri                       <- Future(apiUri)
          hc                        <- ehc
          initializeSessionResponse <- openAdminSession(hc, uri, "admin@localhost", "a")
          _                         <- hc.ask(Http.CloseAll)
        } yield initializeSessionResponse.sessionURI).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      eventualSessionURI.futureValue shouldNot be("")
    }

    "allow the administrator to create and close a session" in {

      val eventualSessionURI: Future[String] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri <- Future(apiUri)
          hc  <- ehc
          isr <- openAdminSession(hc, uri, "admin@localhost", "a")
          rsp <- closeSession(hc, uri, isr.sessionURI)
          _   <- hc.ask(Http.CloseAll)
        } yield rsp).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      eventualSessionURI.futureValue shouldBe "session closed"
    }

    "allow the administrator to query an empty database without crashing" in {

      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri        <- Future(apiUri)
          hc         <- ehc
          isr        <- openAdminSession(hc, uri, "admin@localhost", "a")
          _          <- makeQueryOnSelf(hc, uri, isr.sessionURI, "each([MESSAGEPOSTLABEL])")
          sessionUri <- spawnSession(hc, uri, isr.sessionURI)
          jArray     <- pingUntilPong(hc, uri, sessionUri)
          _          <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      eventualJArray.futureValue.values.length shouldBe 1
    }

    "allow the administrator to make connections" in {

      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri      <- Future(apiUri)
          hc       <- ehc
          isr      <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice    <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
          bob      <- createSRPUser(hc, uri, "bob@testing.com", "bob", "b")
          _        <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          isrA     <- openSRPSession(hc, uri, "alice@testing.com", "a")
          spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
          _        <- getConnectionProfiles(hc, uri, spwnssnA)
          jArray   <- pingUntilPong(hc, uri, spwnssnA)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualJArray) { (ja: JArray) =>
        println(s"Alice's connections: ${pretty(render(ja))}")
        ja.values.length shouldBe 3
      }
    }

    "allow the administrator to make connections (dup)" in {

      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri      <- Future(apiUri)
          hc       <- ehc
          isr      <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice    <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
          bob      <- createSRPUser(hc, uri, "bob@testing.com", "bob", "b")
          _        <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          isrA     <- openSRPSession(hc, uri, "alice@testing.com", "a")
          spwnssnA <- spawnSession(hc, uri, isrA.sessionURI)
          _        <- getConnectionProfiles(hc, uri, spwnssnA)
          jArray   <- pingUntilPong(hc, uri, spwnssnA)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualJArray) { (ja: JArray) =>
        println(s"Alice's connections: ${pretty(render(ja))}")
        ja.values.length shouldBe 3
      }
    }

    "allow administrator to create 300 users" ignore {

      val userCount = 300
      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri     <- Future(apiUri)
          hc      <- ehc
          isr     <- openAdminSession(hc, uri, "admin@localhost", "a")
          _       <- createTestUsers(hc, uri, userCount)(ec, Timeout(7200, SECONDS))
          spwnssn <- spawnSession(hc, uri, isr.sessionURI)
          _       <- getConnectionProfiles(hc, uri, spwnssn)
          jArray  <- pingUntilPong(hc, uri, spwnssn)
          _       <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualJArray) { (ja: JArray) =>
        ja.values.length shouldBe userCount + 2
      }(PatienceConfig(timeout = Span(7200, Seconds)))
    }

    "establish the correct number of connections" in {

      val eventualTuple: Future[(JArray, JArray, JArray)] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri         <- Future(apiUri)
          hc          <- ehc
          isr         <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice       <- createSRPUser(hc, uri, "alice@test.com", "alice", "a")
          bob         <- createSRPUser(hc, uri, "bob@test.com", "bob", "b")
          carol       <- createSRPUser(hc, uri, "carol@test.com", "carol", "c")
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
        } yield (jArrayAlice, jArrayBob, jArrayCarol)).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

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

    "allow the administrator to create users Alice and Bob and allow Alice to make a post which is visible to Bob" in {

      val uid   = "58bbeb443b4c4c0cbda82c99c3178e6e"
      val label = "each([Vogons])"
      val expected =
        ("uid"        -> uid) ~
          ("created"  -> "2016-09-16T17:17:52Z") ~
          ("modified" -> "2016-09-16T17:18:43Z") ~
          ("labels"   -> label) ~
          ("postContent" ->
            ("subject" -> "Like being thrown out of an airlock") ~
              ("text" ->
                """"Oh freddled gruntbuggly,
                  | Thy micturations are to me
                  | As plurdled gabbleblotchits on a lurgid bee.
                  | Groop, I implore thee, my foonting turlingdromes,
                  | And hooptiously drangle me with crinkly bindlewurdles,
                  | Or I will rend thee in the gobberwarts
                  | With my blurglecruncheon, see if I don't!""".stripMargin))

      val expectedJson = compact(render(expected))

      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri                   <- Future(apiUri)
          hc                    <- ehc
          isr                   <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice                 <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
          bob                   <- createSRPUser(hc, uri, "bob@testing.com", "bob", "b")
          _                     <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
          isrA                  <- openSRPSession(hc, uri, "alice@testing.com", "a")
          spwnssnA              <- spawnSession(hc, uri, isrA.sessionURI)
          _                     <- getConnectionProfiles(hc, uri, spwnssnA)
          aliceConnectionsArray <- pingUntilPong(hc, uri, spwnssnA)
          aliceConnections      <- extractConnections(aliceConnectionsArray)
          postConnection        <- Future(aliceConnections.find((connection: Connection) => connection.label == "alice_bob").get)
          _                     <- makePost(hc, uri, isrA.sessionURI, List(postConnection), label, expectedJson, uid)
          isrB                  <- openSRPSession(hc, uri, "bob@testing.com", "b")
          spwnssnB              <- spawnSession(hc, uri, isrB.sessionURI)
          _                     <- getConnectionProfiles(hc, uri, spwnssnB)
          bobConnectionsArray   <- pingUntilPong(hc, uri, spwnssnB)
          bobConnections        <- extractConnections(bobConnectionsArray)
          _                     <- makeQueryOnConnections(hc, uri, spwnssnB, bobConnections, "all([Vogons])")
          queryArray            <- pingUntilPong(hc, uri, spwnssnB)
          _                     <- hc.ask(Http.CloseAll)
        } yield queryArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualJArray) { (jArray: JArray) =>
        println(pretty(render(jArray)))

        val posts = jArray.arr.filter { (value: JValue) =>
          (value \ "msgType").extract[String] == "evalSubscribeResponse"
        }.foldLeft(List.empty[String]) { (acc: List[String], value: JValue) =>
          (value \ "content" \ "pageOfPosts").extract[List[String]] ++ acc
        }

        posts should contain(expectedJson)
      }
    }

    "return balance summary for user" in {

      val eventualJArray: Future[(JArray)] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          _        <- makeBalanceRequest(hc, uri, adminIsr.sessionURI)
          jArray   <- pingUntilTheType(hc, uri, adminIsr.sessionURI, "omniBalanceResponse")
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        val rsp = jArray.arr.head.asInstanceOf[JObject]
        val msgType = (rsp \ "msgType").extract[String]
        val content = (rsp \ "content").extract[OmniBalanceResponse]

        msgType shouldBe "omniBalanceResponse"
        content.amp shouldEqual """0.00000000"""
      }
    }

    "transfer AMPs, check the transaction's confirmation and then return the updated balance" in {
      val eventualJArray: Future[(JArray)] =
        for {
          uri      <- Future(apiUri)
          hc       <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice    <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
          isrA     <- {
                        println(s"User $alice created")
                        openSRPSession(hc, uri, "alice@testing.com", "a")
                      }
          _        <- makeSendAmpsRequest(hc, uri, adminIsr.sessionURI, isrA.sessionURI, "10")
          jArray   <- pingUntilTheType(hc, uri, adminIsr.sessionURI, "omniBalanceResponse")
          _        <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        val rsp = jArray.arr.head.asInstanceOf[JObject]
        val msgType = (rsp \ "msgType").extract[String]
        val content = (rsp \ "content").extract[OmniBalanceResponse]

        msgType shouldBe "omniBalanceResponse"
      }
    }

    /*
     * Query Test Machinery
     *
     * ! HANDLE WITH CARE !
     */

    case class TestPost(uid: String, subject: String, text: String, label: String)

    def constructPostsJsons(testPosts: List[TestPost]) =
      testPosts.map { (current: TestPost) =>
        compact(
          render(
            ("uid" -> current.uid) ~
              ("postContent" ->
                ("subject" -> s"Subject ${current.subject}") ~
                  ("text"  -> s"Text ${current.text}"))))
      }

    def dontPanic(testPosts: List[TestPost], queryLabel: String): Future[(JArray, List[String])] = {
      val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
      (for {
        uri                   <- Future(apiUri)
        hc                    <- ehc
        isr                   <- openAdminSession(hc, uri, "admin@localhost", "a")
        alice                 <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
        bob                   <- createSRPUser(hc, uri, "bob@testing.com", "bob", "b")
        _                     <- makeConnection(hc, uri, isr.sessionURI, alice, bob, "alice_bob")
        isrA                  <- openSRPSession(hc, uri, "alice@testing.com", "a")
        spwnssnA              <- spawnSession(hc, uri, isrA.sessionURI)
        _                     <- getConnectionProfiles(hc, uri, spwnssnA)
        aliceConnectionsArray <- pingUntilPong(hc, uri, spwnssnA)
        aliceConnections      <- extractConnections(aliceConnectionsArray)
        postConnection        <- Future(aliceConnections.find((connection: Connection) => connection.label == "alice_bob").get)
        posts                 <- Future(testPosts)
        postsJsons            <- Future(constructPostsJsons(posts))
        _                     <- makePosts(hc, uri, isrA.sessionURI, List(postConnection), posts.map(_.label), postsJsons, posts.map(_.uid))
        isrB                  <- openSRPSession(hc, uri, "bob@testing.com", "b")
        spwnssnB              <- spawnSession(hc, uri, isrB.sessionURI)
        _                     <- getConnectionProfiles(hc, uri, spwnssnB)
        bobConnectionsArray   <- pingUntilPong(hc, uri, spwnssnB)
        bobConnections        <- extractConnections(bobConnectionsArray)
        _                     <- makeQueryOnConnections(hc, uri, spwnssnB, bobConnections, queryLabel)
        queryArray            <- pingUntilPong(hc, uri, spwnssnB)
        _                     <- hc.ask(Http.CloseAll)
      } yield (queryArray, postsJsons)).recover {
        case e: Throwable =>
          val _ = ehc.flatMap(_.ask(Http.CloseAll))
          throw e
      }
    }

    def extractPosts(jArray: JArray): List[String] =
      jArray.arr.filter { (value: JValue) =>
        (value \ "msgType").extract[String] == "evalSubscribeResponse"
      }.foldLeft(List.empty[String]) { (acc: List[String], value: JValue) =>
        (value \ "content" \ "pageOfPosts").extract[List[String]] ++ acc
      }

    def extractPostUids(jArray: JArray): List[String] = {
      val posts: List[String] = extractPosts(jArray)
      posts.map { (s: String) =>
        (parse(s) \ "uid").extract[String]
      }
    }

    def genPosts(lbls: String*): List[TestPost] = {
      lbls.toList.zipWithIndex.map { case (lbl: String, i: Int) => TestPost("uid_" + i, "Subject " + i, "Text " + i, lbl) } // for HT's enjoyment :-)
    }

    "respond with expected query results (each)" in {

      val testPosts: List[TestPost] = genPosts("each([Vogon])", "each([Vogon],[Dent])", "each([Vogon])")

      val query: String = "each([Dent])"

      val results: Future[(JArray, List[String])] = dontPanic(testPosts, query)

      whenReady(results) { (tuple: (JArray, List[String])) =>
        println(pretty(render(tuple._1)))
        val postUids: List[String] = extractPostUids(tuple._1)
        postUids should contain only "uid_1"
      }
    }

    "respond with expected query results (any)" in {

      val testPosts: List[TestPost] = genPosts("all([Vogon])",
                                               "all([Vogon])",
                                               "any([Vogon],[Dent],[Marvin])",
                                               "all([Dent])",
                                               "all([Dent],[Marvin])",
                                               "all([Dent],[Vogon])")

      val query: String = "any([Dent],[Vogon])"

      val results: Future[(JArray, List[String])] = dontPanic(testPosts, query)

      whenReady(results) { (tuple: (JArray, List[String])) =>
        println(pretty(render(tuple._1)))
        val postUids: List[String] = extractPostUids(tuple._1)
        postUids should contain only ("uid_0", "uid_1", "uid_2", "uid_3")
      }
    }

    "respond with expected query results (all)" in {

      val testPosts: List[TestPost] = genPosts("any([Vogon])",
                                               "any([Vogon])",
                                               "each([Vogon],[Dent],[Marvin])",
                                               "any([Dent],[Vogon])",
                                               "any([Dent],[Marvin])",
                                               "all([Vogon],[Dent])",
                                               "all([Dent],[Vogon])")

      val query: String = "all([Dent],[Vogon])"

      val results: Future[(JArray, List[String])] = dontPanic(testPosts, query)

      whenReady(results) { (tuple: (JArray, List[String])) =>
        println(pretty(render(tuple._1)))
        val postUids: List[String] = extractPostUids(tuple._1)
        postUids should contain only ("uid_5", "uid_6")
      }
    }

    "return evalSubscribeResponse when querying using 'any', 'each' or 'all'" in {

      val eventualJArray: Future[JArray] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri      <- Future(apiUri)
          hc       <- ehc
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
          _        <- makeQueryOnSelf(hc, uri, ssn, "each([MESSAGEPOSTLABEL])")
          _        <- makeQueryOnSelf(hc, uri, ssn, "any([MESSAGEPOSTLABEL])")
          _        <- makeQueryOnSelf(hc, uri, ssn, "all([MESSAGEPOSTLABEL])")
          jArray   <- pingUntilPong(hc, uri, ssn)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualJArray) { (ja: JArray) =>
        val rsp     = ja.arr.head.asInstanceOf[JObject]
        val msgType = (rsp \ "msgType").extract[String]
        ja.arr.length shouldBe 1
        msgType shouldBe "sessionPong"
      }
    }

    "return evalSubscribeError when querying not using 'any', 'each' or 'all'" in {

      val eventualJArray: Future[(JArray)] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri      <- Future(apiUri)
          hc       <- ehc
          adminIsr <- openAdminSession(hc, uri, "admin@localhost", "a")
          ssn      <- spawnSession(hc, uri, adminIsr.sessionURI)
          _        <- makeQueryOnSelf(hc, uri, ssn, "lordfarquad([MESSAGEPOSTLABEL])")
          jArray   <- pingUntilPong(hc, uri, ssn)
          _        <- hc.ask(Http.CloseAll)
        } yield jArray).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

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

      val eventualString: Future[String] = {
        val ehc: Future[ActorRef] = eventualHostConnector(system, apiUri.effectivePort, sslEngineProvider)
        (for {
          uri   <- Future(apiUri)
          hc    <- ehc
          alice <- createSRPUser(hc, uri, "alice@test.com", "alice", "a")
          isrA  <- openSRPSession(hc, uri, "alice@test.com", "a")
          _     <- startCam(hc, uri, isrA.sessionURI)
          _     <- pingUntilPong(hc, uri, isrA.sessionURI)
          s     <- stopCam(hc, uri, isrA.sessionURI)
          _     <- hc.ask(Http.CloseAll)
        } yield s).recover {
          case e: Throwable =>
            val _ = ehc.flatMap(_.ask(Http.CloseAll))
            throw e
        }
      }

      whenReady(eventualString) { (s: String) =>
        println(s)
        s shouldNot be("")
      }
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
    Thread.sleep(15000L)
  }

  override def afterEach(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }
}
