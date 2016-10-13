package com.synereo.worlock

import java.net.InetSocketAddress

import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.{Distributed, Headed, Headless}
import com.biosimilarity.evaluator.spray.ApiTests
import com.biosimilarity.evaluator.util._
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.synereo.worlock.test._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.time.{Second, Seconds, Span}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Random, Success, Try}

class SimpleDualNodeTest
    extends ApiTests(Uri("https://localhost:9876/api"), trustfulClientSSLEngineProvider)
    with BeforeAndAfterAll
    with IntegrationPatience {

  def containerName(): String = s"SimpleDualNodeTest-${java.util.UUID.randomUUID().toString}"

  val logger: Logger = LoggerFactory.getLogger(classOf[SimpleDualNodeTest])

  implicit val timeout: Timeout = Timeout(FiniteDuration(360, SECONDS))

  override implicit val patienceConfig = PatienceConfig(timeout = Span(360, Seconds), interval = Span(1, Second))

  val dockerClient: Try[DockerClient] = getDockerClient()

  var networkInfo: Option[(DockerClient, DockerNetwork)] = None

  var containerInfo: Option[(DockerClient, List[CreateContainerResponse])] = None

  val dockerTestNetwork: DockerNetwork = DockerNetwork("synereo", "10.100.101.0/24")

  override def beforeAll(): Unit = {
    networkInfo = (for {
      client  <- dockerClient
      network <- createOrGetNetwork(client, dockerTestNetwork)
    } yield (client, network)) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Created network:", x._2.name))
        Some(x)
      case Failure(exception) => throw exception
    }
  }

  override def beforeEach(): Unit = {
    val headlessName: String = containerName() + s"-$Headless"
    val headedName: String   = containerName() + s"-$Headed"
    val rs: Iterator[Int]    = Random.shuffle(1 to 254).toIterator
    lazy val headlessNode: Node = HeadlessNode(name = headlessName,
                                               deploymentMode = Distributed,
                                               address = new InetSocketAddress(s"10.100.101.${rs.next()}", 5672),
                                               dslCommLinkServer = headlessNode,
                                               dslCommLinkClients = List(headedNode),
                                               dslEvaluator = headedNode,
                                               dslEvaluatorPreferredSupplier = headedNode,
                                               bFactoryCommLinkServer = headedNode,
                                               bFactoryCommLinkClient = headlessNode,
                                               bFactoryEvaluator = headlessNode)
    lazy val headedNode: Node = HeadedNode(name = headedName,
                                           deploymentMode = Distributed,
                                           address = new InetSocketAddress(s"10.100.101.${rs.next()}", 5672),
                                           dslCommLinkServer = headedNode,
                                           dslCommLinkClients = List(headlessNode),
                                           dslEvaluator = headlessNode,
                                           dslEvaluatorPreferredSupplier = headlessNode,
                                           bFactoryCommLinkServer = headlessNode,
                                           bFactoryCommLinkClient = headedNode,
                                           bFactoryEvaluator = headedNode,
                                           serverPort = 8567,
                                           exposedServerPort = Some(8567),
                                           serverSSLPort = 9876,
                                           exposedServerSSLPort = Some(9876))
    containerInfo = (for {
      client            <- dockerClient
      network           <- networkInfo.map(_._2).toTry
      headlessContainer <- createContainer(client, network, headlessNode)
      headedContainer   <- createContainer(client, network, headedNode)
      _                 <- startContainer(client, headlessContainer.getId)
      _                 <- startContainer(client, headedContainer.getId)
    } yield (client, List(headlessContainer, headedContainer))) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Created container:", headlessName))
        logger.info("%-24s %s".format("Created container:", headedName))
        Some(x)
      case Failure(exception) => throw exception
    }
    logger.info("%-24s %s".format("Waiting for server:", headedName))
    spinwaitOnServer(system, apiUri, FiniteDuration(5, SECONDS), 20)(ec, system.scheduler, timeout)
    logger.info("%-24s %s".format("Waiting for quiescence:", headedName))
    Thread.sleep(5000L)
    logger.info("%-24s %s".format("Starting test:", headedName))
  }

  override def afterEach(): Unit = {
    containerInfo.foreach {
      case (client, cs) =>
        logger.info("%s".format("Test complete"))
        cs.foreach { (container: CreateContainerResponse) =>
          val name: String = client.inspectContainerCmd(container.getId).exec().getName.substring(1)
          client.stopContainerCmd(container.getId).exec()
          logger.info("%-24s %s".format("Stopped container:", name))
          client.removeContainerCmd(container.getId).exec()
          logger.info("%-24s %s".format("Destroyed container:", name))
        }
    }
    containerInfo = None
  }

  override def afterAll(): Unit = {
    networkInfo.foreach {
      case (client, network) =>
        getNetworkId(client, network).foreach { (s: String) =>
          client.removeNetworkCmd(s).exec()
          logger.info("%-24s %s".format("Destroyed network:", network.name))
        }
    }
    networkInfo = None
  }
}
