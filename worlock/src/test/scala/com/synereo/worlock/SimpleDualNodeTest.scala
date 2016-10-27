package com.synereo.worlock

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicBoolean

import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.{Distributed, Headed, Headless}
import com.biosimilarity.evaluator.spray.ApiTests
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.synereo.worlock.test._
import org.scalatest.time.{Second, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, Failed, Succeeded}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Random, Success, Try}

class SimpleDualNodeTest extends ApiTests(Uri("https://localhost:9876/api"), trustfulClientSSLEngineProvider) with BeforeAndAfterAll {

  val logger: Logger = LoggerFactory.getLogger(classOf[SimpleDualNodeTest])

  val timeoutLength: Int = 120

  implicit val timeout: Timeout = Timeout(FiniteDuration(timeoutLength, SECONDS))

  override implicit val patienceConfig = PatienceConfig(timeout = Span(timeoutLength, Seconds), interval = Span(1, Second))

  def containerName(): String = s"SimpleDualNodeTest-${java.util.UUID.randomUUID().toString}"

  val deleteNetworkAfterTests: AtomicBoolean = new AtomicBoolean(false)

  val client: DockerClient = getDockerClient().get

  val testNetwork: DockerNetwork = DockerNetwork("synereo", "10.100.101.0/24")

  override def beforeAll(): Unit =
    createOrGetNetwork(client, testNetwork) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Using network:", x.name))
      case Failure(exception) =>
        throw exception
    }

  override def withFixture(test: NoArgTest) = {

    val destroyContainerAfterTest: AtomicBoolean = new AtomicBoolean(true)

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

    logger.info("%-24s %s".format("Preparing test:", test.name))

    val containerInfo: List[CreateContainerResponse] = setupContainers(client, testNetwork, List(headlessNode, headedNode), ae).get

    containerInfo.foreach { (response: CreateContainerResponse) =>
      logger.info("%-24s %s".format("Started container:", client.inspectContainerCmd(response.getId).exec().getName.substring(1)))
    }

    try {
      logger.info("%-24s %s".format("Waiting for server:", headedName))
      spinwaitOnServer(system, apiUri, FiniteDuration(5, SECONDS), 20)(ec, system.scheduler, timeout)

      logger.info("%-24s %s".format("Waiting for quiescence:", headedName))
      Thread.sleep(45000L)

      logger.info("%-24s %s".format("Starting test:", test.name))

      super.withFixture(test) match {
        case Succeeded =>
          logger.info("%-24s %s".format("Test passed:", test.name))
          Succeeded
        case failed: Failed =>
          logger.error("%-24s %s".format("Test failed:", test.name))
          destroyContainerAfterTest.set(false)
          deleteNetworkAfterTests.set(false)
          failed
        case other =>
          logger.error("%-24s %s".format("Unexpected test result", test.name))
          destroyContainerAfterTest.set(false)
          deleteNetworkAfterTests.set(false)
          other
      }
    } finally {
      logger.info("%-24s %s".format("Tearing down:", test.name))
      teardownContainers(client, containerInfo, destroyContainerAfterTest.get).foreach { (usedContainers: List[UsedContainer]) =>
        usedContainers.foreach {
          case UsedContainer(name, _, destroyed) =>
            logger.info("%-24s %s".format("Stopped container:", name))
            if (destroyed)
              logger.info("%-24s %s".format("Destroyed container:", name))
            else
              logger.info("%-24s %s".format("Saved container:", name))
        }
      }
    }
  }

  override def afterAll(): Unit = {
    if (deleteNetworkAfterTests.get()) {
      getNetworkId(client, testNetwork).foreach { (s: String) =>
        client.removeNetworkCmd(s).exec()
        logger.info("%-24s %s".format("Destroyed network:", testNetwork.name))
      }
    }
    client.close()
  }
}
