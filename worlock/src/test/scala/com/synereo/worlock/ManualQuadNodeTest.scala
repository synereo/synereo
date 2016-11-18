package com.synereo.worlock

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.Distributed
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.synereo.worlock.test._
import org.json4s.JArray
import org.json4s.jackson.JsonMethods._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Second, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.{HttpResponse, StatusCodes, Uri}

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try

class ManualQuadNodeTest extends WordSpec with ApiClient with Matchers with ScalaFutures with BeforeAndAfterAll {

  val system: ActorSystem                   = ActorSystem()
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout             = Timeout(FiniteDuration(360, SECONDS))
  implicit override val patienceConfig      = PatienceConfig(timeout = Span(360, Seconds), interval = Span(1, Second))
  implicit val logger: Logger               = LoggerFactory.getLogger(classOf[ManualQuadNodeTest])

  val client: DockerClient       = getDockerClient().get
  val testNetwork: DockerNetwork = DockerNetwork("synereo", "10.100.101.0/24")
  val apiUri                     = Uri("https://localhost:9876/api")
  val ae: Map[String, String]    = Map("TWEET_LEVEL" -> "debug")

  lazy val headlessNode01: Node = HeadlessNode(name = "headlessNode01",
                                               deploymentMode = Distributed,
                                               address = new InetSocketAddress(s"10.100.101.2", 5672),
                                               dslCommLinkServer = headlessNode01,
                                               dslCommLinkClients = List(headedNode01),
                                               dslEvaluator = headedNode01,
                                               dslEvaluatorPreferredSupplier = headedNode01,
                                               bFactoryCommLinkServer = headedNode01,
                                               bFactoryCommLinkClient = headlessNode01,
                                               bFactoryEvaluator = headlessNode01,
                                               exposedDebugPort = Some(5005),
                                               exposedMongoPort = Some(27017),
                                               exposedRabbitManagementPort = Some(55672),
                                               suspendForDebugger = false)

  lazy val headedNode01: Node = HeadedNode(name = "headedNode01",
                                           deploymentMode = Distributed,
                                           address = new InetSocketAddress(s"10.100.101.4", 5672),
                                           dslCommLinkServer = headedNode01,
                                           dslCommLinkClients = List(headlessNode01),
                                           dslEvaluator = headlessNode01,
                                           dslEvaluatorPreferredSupplier = headlessNode01,
                                           bFactoryCommLinkServer = headlessNode01,
                                           bFactoryCommLinkClient = headedNode01,
                                           bFactoryEvaluator = headlessNode01,
                                           serverPort = 8567,
                                           exposedServerPort = Some(8567),
                                           serverSSLPort = 9876,
                                           exposedServerSSLPort = Some(9876),
                                           exposedDebugPort = Some(5007),
                                           exposedMongoPort = Some(47017),
                                           exposedRabbitManagementPort = Some(55674),
                                           suspendForDebugger = false)

  lazy val headlessNode02: Node = HeadlessNode(name = "headlessNode02",
                                               deploymentMode = Distributed,
                                               address = new InetSocketAddress(s"10.100.101.3", 5672),
                                               dslCommLinkServer = headlessNode02,
                                               dslCommLinkClients = List(headedNode02, headlessNode01),
                                               dslEvaluator = headedNode02,
                                               dslEvaluatorPreferredSupplier = headedNode02,
                                               bFactoryCommLinkServer = headedNode02,
                                               bFactoryCommLinkClient = headlessNode02,
                                               bFactoryEvaluator = headlessNode02,
                                               exposedDebugPort = Some(5006),
                                               exposedMongoPort = Some(37017),
                                               exposedRabbitManagementPort = Some(55673),
                                               suspendForDebugger = false)

  lazy val headedNode02: Node = HeadedNode(name = "headedNode02",
                                           deploymentMode = Distributed,
                                           address = new InetSocketAddress(s"10.100.101.5", 5672),
                                           dslCommLinkServer = headedNode02,
                                           dslCommLinkClients = List(headlessNode02),
                                           dslEvaluator = headlessNode02,
                                           dslEvaluatorPreferredSupplier = headlessNode02,
                                           bFactoryCommLinkServer = headlessNode02,
                                           bFactoryCommLinkClient = headedNode02,
                                           bFactoryEvaluator = headlessNode02,
                                           serverPort = 8567,
                                           exposedServerPort = Some(9567),
                                           serverSSLPort = 9876,
                                           exposedServerSSLPort = Some(10876),
                                           exposedDebugPort = Some(5008),
                                           exposedMongoPort = Some(57017),
                                           exposedRabbitManagementPort = Some(55675),
                                           suspendForDebugger = false)

  def setupNetwork(): Unit = setupTestNetwork(client, testNetwork)

  def setupContainers(): Try[List[CreateContainerResponse]] =
    for {
      network             <- Try(testNetwork)
      headlessContainer01 <- createContainer(client, network, headlessNode01, ae)
      headlessContainer02 <- createContainer(client, network, headlessNode02, ae)
      headedContainer01   <- createContainer(client, network, headedNode01, ae)
      headedContainer02   <- createContainer(client, network, headedNode02, ae)
      _                   <- startContainer(client, headlessContainer01.getId)
      _                   <- tryLogger("%-24s %s".format("Started container:", headlessNode01.name))
      _                   <- startContainer(client, headlessContainer02.getId)
      _                   <- tryLogger("%-24s %s".format("Started container:", headlessNode02.name))
      _                   <- startContainer(client, headedContainer01.getId)
      _                   <- tryLogger("%-24s %s".format("Started container:", headedNode01.name))
      _                   <- startContainer(client, headedContainer02.getId)
      _                   <- tryLogger("%-24s %s".format("Started container:", headedNode02.name))
    } yield List(headlessContainer01, headlessContainer02, headedContainer01, headlessContainer02)

  val aliceAgentURI: AtomicReference[String] = new AtomicReference[String]("")
  val bobAgentURI: AtomicReference[String]   = new AtomicReference[String]("")
  val carolAgentURI: AtomicReference[String] = new AtomicReference[String]("")

  "The API" should {

    "allow the administrator to create two users, Alice and Bob, and connect them" in {

      val eventualHttpResponse: Future[HttpResponse] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openAdminSession(hc, uri, "admin@localhost", "a")
          alice                     <- createSRPUser(hc, uri, "alice@testing.com", "alice", "a")
          _                         <- Future(aliceAgentURI.set(alice))
          bob                       <- createSRPUser(hc, uri, "bob@testing.com", "bob", "b")
          _                         <- Future(bobAgentURI.set(bob))
          response                  <- makeConnection(hc, uri, initializeSessionResponse.sessionURI, alice, bob, "alice_bob")
          _                         <- hc.ask(Http.CloseAll)
        } yield response

      whenReady(eventualHttpResponse) { (response: HttpResponse) =>
        response.status shouldEqual StatusCodes.OK
      }
    }

    "allow Alice to get her connections" in {

      val eventualJArray: Future[JArray] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openSRPSession(hc, uri, "alice@testing.com", "a")
          sessionUri                <- spawnSession(hc, uri, initializeSessionResponse.sessionURI)
          _                         <- getConnectionProfiles(hc, uri, sessionUri)
          jArray                    <- pingUntilPong(hc, uri, sessionUri)
          _                         <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        println(pretty(render(jArray)))
        jArray.arr.length shouldBe 3
      }
    }

    "allow the administrator to create another user, Carol, and connect her to Alice" in {

      val eventualHttpResponse: Future[HttpResponse] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openAdminSession(hc, uri, "admin@localhost", "a")
          carol                     <- createSRPUser(hc, uri, "carol@testing.com", "carol", "c")
          _                         <- Future(carolAgentURI.set(carol))
          response                  <- makeConnection(hc, uri, initializeSessionResponse.sessionURI, carol, aliceAgentURI.get, "alice_carol")
          _                         <- hc.ask(Http.CloseAll)
        } yield response

      whenReady(eventualHttpResponse) { (response: HttpResponse) =>
        response.status shouldEqual StatusCodes.OK
      }
    }

    "allow Carol to get her connections" in {

      val eventualJArray: Future[JArray] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openSRPSession(hc, uri, "carol@testing.com", "c")
          sessionUri                <- spawnSession(hc, uri, initializeSessionResponse.sessionURI)
          _                         <- getConnectionProfiles(hc, uri, sessionUri)
          jArray                    <- pingUntilPong(hc, uri, sessionUri)
          _                         <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        println(pretty(render(jArray)))
        jArray.arr.length shouldBe 3
      }
    }

    "allow Bob to get his connections" in {

      val eventualJArray: Future[JArray] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openSRPSession(hc, uri, "bob@testing.com", "b")
          sessionUri                <- spawnSession(hc, uri, initializeSessionResponse.sessionURI)
          _                         <- getConnectionProfiles(hc, uri, sessionUri)
          jArray                    <- pingUntilPong(hc, uri, sessionUri)
          _                         <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        println(pretty(render(jArray)))
        jArray.arr.length shouldBe 3
      }
    }

    "again allow Alice to get her connections" in {

      val eventualJArray: Future[JArray] =
        for {
          uri                       <- Future(apiUri)
          hc                        <- eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
          initializeSessionResponse <- openSRPSession(hc, uri, "alice@testing.com", "a")
          sessionUri                <- spawnSession(hc, uri, initializeSessionResponse.sessionURI)
          _                         <- getConnectionProfiles(hc, uri, sessionUri)
          jArray                    <- pingUntilPong(hc, uri, sessionUri)
          _                         <- hc.ask(Http.CloseAll)
        } yield jArray

      whenReady(eventualJArray) { (jArray: JArray) =>
        println(pretty(render(jArray)))
        jArray.arr.length shouldBe 4
      }
    }
  }
}
