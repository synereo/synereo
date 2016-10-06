package com.synereo.worlock

import com.biosimilarity.evaluator.spray.ApiTests
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.synereo.worlock.test._
import org.scalatest.concurrent.IntegrationPatience
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Success}

class SimpleColocatedTest extends ApiTests(Uri("https://localhost:9876/api"), trustfulClientSSLEngineProvider) with IntegrationPatience {

  val logger: Logger = LoggerFactory.getLogger(classOf[SimpleColocatedTest])

  def containerName(): String = s"SimpleColocatedTest-${java.util.UUID.randomUUID().toString}"

  var containerInfo: Option[(DockerClient, CreateContainerResponse)] = None

  override def beforeEach(): Unit = {
    val name: String = containerName()
    containerInfo = createColocatedTestContainer(name, "127.0.0.1", 5672, apiUri.effectivePort) match {
      case Success(x) =>
        logger.info("%-24s %s".format("Created container:", name))
        Some(x)
      case Failure(exception) => throw exception
    }
    logger.info("%-24s %s".format("Waiting for server:", name))
    spinwaitOnServer(system, apiUri, FiniteDuration(5, SECONDS), 20)(ec, system.scheduler, timeout)
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
}
