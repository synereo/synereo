package com.synereo

import java.io.File
import java.nio.file.Paths

import com.biosimilarity.evaluator.BuildInfo
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model._
import com.github.dockerjava.core.command.BuildImageResultCallback
import com.github.dockerjava.core.{DefaultDockerClientConfig, DockerClientBuilder}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConversions._
import scala.util.Try

package object worlock {

  val logger: Logger = LoggerFactory.getLogger("worlock")

  val defaultDockerClientConfig: DefaultDockerClientConfig =
    DefaultDockerClientConfig.createDefaultConfigBuilder().withDockerHost("tcp://localhost:2376").build()

  val defaultBuildImageResultCallback = new BuildImageResultCallback() {
    override def onNext(item: BuildResponseItem): Unit = {
      logger.info(item.getStream.replaceAll("\n", ""))
      super.onNext(item)
    }
  }

  val baseImagePath: File = Paths.get(System.getProperty("user.dir")).resolve("src/main/docker/base").toFile

  def colocatedEnvironment(ip: String, port: Int): Try[Map[String, String]] =
    Try {
      Map[String, String]("DEPLOYMENT_MODE"                       -> "colocated",
                          "DSL_COMM_LINK_CLIENT_HOSTS"            -> s"$ip:${port.toString}",
                          "DSL_EVALUATOR_HOST"                    -> ip,
                          "DSL_EVALUATOR_PORT"                    -> port.toString,
                          "DSL_EVALUATOR_PREFERRED_SUPPLIER_HOST" -> ip,
                          "DSL_EVALUATOR_PREFERRED_SUPPLIER_PORT" -> port.toString,
                          "BFACTORY_COMM_LINK_SERVER_HOST"        -> ip,
                          "BFACTORY_COMM_LINK_SERVER_PORT"        -> port.toString,
                          "BFACTORY_COMM_LINK_CLIENT_HOST"        -> ip,
                          "BFACTORY_COMM_LINK_CLIENT_PORT"        -> port.toString,
                          "BFACTORY_EVALUATOR_HOST"               -> ip,
                          "BFACTORY_EVALUATOR_PORT"               -> port.toString)
    }

  def colocatedPortBindings(apiPort: Int): Try[Ports] =
    Try {
      val tcp9876 = ExposedPort.tcp(apiPort)
      val ports   = new Ports()
      ports.bind(tcp9876, Binding.bindPort(apiPort))
      ports
    }

  private def environmentMapToList(env: Map[String, String]): List[String] =
    env.foldLeft(List.empty[String]) { (accum: List[String], curr: (String, String)) =>
      s"${curr._1}=${curr._2}" :: accum
    }

  def getDockerClient(config: DefaultDockerClientConfig = defaultDockerClientConfig): Try[DockerClient] =
    Try(DockerClientBuilder.getInstance(config).build())

  def buildContainer(client: DockerClient, dir: File, callback: BuildImageResultCallback = defaultBuildImageResultCallback): Try[String] =
    Try(client.buildImageCmd(dir).exec(callback).awaitImageId())

  def createContainer(client: DockerClient, name: String, environment: Map[String, String], binds: Ports): Try[CreateContainerResponse] =
    Try {
      client
        .createContainerCmd(s"gloseval:${BuildInfo.version}")
        .withName(name)
        .withEnv(environmentMapToList(environment))
        .withPortBindings(binds)
        .exec()
    }

  def startContainer(client: DockerClient, containerId: String): Try[Unit] =
    Try(client.startContainerCmd(containerId).exec())

  def createColocatedTestContainer(name: String, ip: String, rabbitPort: Int, apiPort: Int): Try[(DockerClient, CreateContainerResponse)] =
    for {
      client      <- getDockerClient()
      environment <- colocatedEnvironment(ip, rabbitPort)
      bindings    <- colocatedPortBindings(apiPort)
      container   <- createContainer(client, name, environment, bindings)
      _           <- startContainer(client, container.getId)
    } yield (client, container)
}
