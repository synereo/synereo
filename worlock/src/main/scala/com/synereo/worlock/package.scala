package com.synereo

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
import com.github.dockerjava.core.{DefaultDockerClientConfig, DockerClientBuilder}

import scala.collection.JavaConversions._
import scala.util.Try

package object worlock extends Network {

  val defaultDockerClientConfig: DefaultDockerClientConfig =
    DefaultDockerClientConfig.createDefaultConfigBuilder().withDockerHost("tcp://localhost:2376").build()

  def getDockerClient(config: DefaultDockerClientConfig = defaultDockerClientConfig): Try[DockerClient] =
    Try(DockerClientBuilder.getInstance(config).build())

  private def environmentMapToList(env: Map[String, String]): List[String] =
    env.foldLeft(List.empty[String]) { (accum: List[String], curr: (String, String)) =>
      s"${curr._1}=${curr._2}" :: accum
    }

  def createContainer[T](client: DockerClient, node: T)(implicit c: Containable[T]): Try[CreateContainerResponse] =
    Try {
      client
        .createContainerCmd(c.imageName)
        .withName(c.getContainerName(node))
        .withEnv(environmentMapToList(c.getEnvironment(node)))
        .withPortBindings(c.getPortBindings(node))
        .exec()
    }

  def startContainer(client: DockerClient, containerId: String): Try[Unit] =
    Try(client.startContainerCmd(containerId).exec())

  def createAndStartContainer[T](node: T)(implicit c: Containable[T]): Try[(DockerClient, CreateContainerResponse)] =
    for {
      client    <- getDockerClient()
      container <- createContainer(client, node)
      _         <- startContainer(client, container.getId)
    } yield (client, container)
}
