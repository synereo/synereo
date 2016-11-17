package com.synereo.worlock

import java.net.InetSocketAddress

import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.Colocated
import com.biosimilarity.evaluator.spray.ApiTests
import com.biosimilarity.evaluator.util._
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.synereo.worlock.test._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.time.{Second, Seconds, Span}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Success, Try}

class SimpleColocatedTest extends ApiTests(Uri("https://localhost:9876/api"), trustfulClientSSLEngineProvider) with BeforeAndAfterAll {

  def containerName(): String = s"SimpleColocatedTest-${java.util.UUID.randomUUID().toString}"

  val logger: Logger = LoggerFactory.getLogger(classOf[SimpleColocatedTest])

  val timeoutLength: Int = 120

  implicit val timeout: Timeout = Timeout(FiniteDuration(timeoutLength, SECONDS))

  override implicit val patienceConfig = PatienceConfig(timeout = Span(timeoutLength, Seconds), interval = Span(1, Second))

  val dockerClient: Try[DockerClient] = getDockerClient()

  var networkInfo: Option[(DockerClient, DockerNetwork)] = None

  var containerInfo: Option[(DockerClient, CreateContainerResponse)] = None

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
    val name: String = containerName()
    lazy val colocatedNode: Node = HeadedNode(name = name,
                                              deploymentMode = Colocated,
                                              address = new InetSocketAddress("10.100.101.10", 6672),
                                              dslCommLinkServer = colocatedNode,
                                              dslCommLinkClients = List(colocatedNode),
                                              dslEvaluator = colocatedNode,
                                              dslEvaluatorPreferredSupplier = colocatedNode,
                                              bFactoryCommLinkServer = colocatedNode,
                                              bFactoryCommLinkClient = colocatedNode,
                                              bFactoryEvaluator = colocatedNode,
                                              serverPort = 8567,
                                              exposedServerPort = Some(8567),
                                              serverSSLPort = 9876,
                                              exposedServerSSLPort = Some(9876),
                                              exposedDebugPort = Some(5005),
                                              exposedMongoPort = Some(27017),
                                              exposedRabbitManagementPort = Some(55672),
                                              suspendForDebugger = false)
    containerInfo = (for {
      client    <- dockerClient
      network   <- networkInfo.map(_._2).toTry
      container <- createContainer(client, network, colocatedNode)
      _         <- startContainer(client, container.getId)
    } yield (client, container)) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Created container:", name))
        Some(x)
      case Failure(exception) => throw exception
    }
    logger.info("%-24s %s".format("Waiting for server:", name))
    spinwaitOnServer(system, apiUri, FiniteDuration(5, SECONDS), 20)(ec, system.scheduler, timeout)
    Thread.sleep(5000L)
    logger.info("%-24s %s".format("Starting test:", name))
  }

  override def afterEach(): Unit = {
    containerInfo.foreach {
      case (client, container) =>
        val name: String = client.inspectContainerCmd(container.getId).exec().getName.substring(1)
        logger.info("%-24s %s".format("Test complete:", name))
        client.stopContainerCmd(container.getId).exec()
        logger.info("%-24s %s".format("Stopped container:", name))
        client.removeContainerCmd(container.getId).exec()
        logger.info("%-24s %s".format("Destroyed container:", name))
      case _ =>
        ()
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
