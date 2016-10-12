package com.synereo

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.{CreateContainerResponse, CreateNetworkResponse}
import com.github.dockerjava.api.model.Network
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

  def createNetwork(client: DockerClient, name: String, subnet: String): Try[CreateNetworkResponse] =
    Try {
      client.createNetworkCmd().withIpam(new Network.Ipam().withConfig(new Network.Ipam.Config().withSubnet(subnet))).withName(name).exec()
    }

  def createContainer[T](client: DockerClient, networkResponse: CreateNetworkResponse, node: T)(
      implicit c: Containable[T]): Try[CreateContainerResponse] =
    Try {
      val networkName: String = client.inspectNetworkCmd().withNetworkId(networkResponse.getId).exec().getName
      client
        .createContainerCmd(c.imageName)
        .withName(c.getContainerName(node))
        .withEnv(environmentMapToList(c.getEnvironment(node)))
        .withPortBindings(c.getPortBindings(node))
        .withNetworkMode(networkName)
        .withIpv4Address(c.getIpv4Address(node))
        .exec()
    }

  def startContainer(client: DockerClient, containerId: String): Try[Unit] =
    Try(client.startContainerCmd(containerId).exec())

  def createAndStartContainer[T](node: T)(implicit c: Containable[T]): Try[(DockerClient, CreateContainerResponse)] =
    for {
      client    <- getDockerClient()
      network   <- createNetwork(client, "synereo", "10.100.101.0/24")
      container <- createContainer(client, network, node)
      _         <- startContainer(client, container.getId)
    } yield (client, container)
}
