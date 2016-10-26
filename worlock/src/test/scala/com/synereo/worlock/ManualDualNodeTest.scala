package com.synereo.worlock

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.{Distributed, Headed, Headless}
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

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Random, Success, Try}

class ManualDualNodeTest extends WordSpec with ApiClient with Matchers with ScalaFutures with BeforeAndAfterAll {

  val system: ActorSystem = ActorSystem()

  implicit val ec = system.dispatcher

  implicit val timeout: Timeout = Timeout(FiniteDuration(360, SECONDS))

  implicit override val patienceConfig = PatienceConfig(timeout = Span(360, Seconds), interval = Span(1, Second))

  val logger: Logger = LoggerFactory.getLogger(classOf[ManualDualNodeTest])

  val apiUri = Uri("https://localhost:9876/api")

  val client: DockerClient = getDockerClient().get

  val testNetwork: DockerNetwork = DockerNetwork("synereo", "10.100.101.0/24")

  def setupNetwork(): Unit =
    createOrGetNetwork(client, testNetwork) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Using network:", x.name))
      case Failure(exception) =>
        throw exception
    }

  def containerName(): String = s"ManualDualNodeTest-${java.util.UUID.randomUUID().toString}"

  def tryLogger(msg: String): Try[Unit] = Try(logger.info(msg))

  val headlessName: String    = containerName() + s"-$Headless"
  val headedName: String      = containerName() + s"-$Headed"
  val rs: Iterator[Int]       = Random.shuffle(2 to 254).toIterator
  val ae: Map[String, String] = Map("TWEET_LEVEL" -> "warning")

  lazy val headlessNode: Node = HeadlessNode(name = headlessName,
                                             deploymentMode = Distributed,
                                             address = new InetSocketAddress(s"10.100.101.${rs.next()}", 5672),
                                             dslCommLinkServer = headlessNode,
                                             dslCommLinkClients = List(headedNode),
                                             dslEvaluator = headedNode,
                                             dslEvaluatorPreferredSupplier = headedNode,
                                             bFactoryCommLinkServer = headedNode,
                                             bFactoryCommLinkClient = headlessNode,
                                             bFactoryEvaluator = headlessNode,
                                             exposedDebugPort = Some(5005),
                                             exposedMongoPort = Some(37017),
                                             exposedRabbitManagementPort = Some(55672))

  lazy val headedNode: Node = HeadedNode(name = headedName,
                                         deploymentMode = Distributed,
                                         address = new InetSocketAddress(s"10.100.101.${rs.next()}", 5672),
                                         dslCommLinkServer = headedNode,
                                         dslCommLinkClients = List(headlessNode),
                                         dslEvaluator = headlessNode,
                                         dslEvaluatorPreferredSupplier = headlessNode,
                                         bFactoryCommLinkServer = headlessNode,
                                         bFactoryCommLinkClient = headedNode,
                                         bFactoryEvaluator = headlessNode,
                                         serverPort = 8567,
                                         exposedServerPort = Some(8567),
                                         serverSSLPort = 9876,
                                         exposedServerSSLPort = Some(9876),
                                         exposedDebugPort = Some(5006),
                                         exposedMongoPort = Some(27017),
                                         exposedRabbitManagementPort = Some(55673))

  def setupContainers(): Try[List[CreateContainerResponse]] =
    for {
      network           <- Try(testNetwork)
      headlessContainer <- createContainer(client, network, headlessNode, ae)
      headedContainer   <- createContainer(client, network, headedNode, ae)
      _                 <- startContainer(client, headlessContainer.getId)
      _                 <- tryLogger("%-24s %s".format("Started container:", headlessName))
      _                 <- startContainer(client, headedContainer.getId)
      _                 <- tryLogger("%-24s %s".format("Started container:", headedName))
    } yield List(headlessContainer, headedContainer)

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
          alice                     <- createSRPUser(hc, "alice@testing.com", "alice", "a")
          _                         <- Future(aliceAgentURI.set(alice))
          bob                       <- createSRPUser(hc, "bob@testing.com", "bob", "b")
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
          carol                     <- createSRPUser(hc, "carol@testing.com", "carol", "c")
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
