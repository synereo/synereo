package com.synereo

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.{CreateContainerResponse, CreateNetworkResponse}
import com.github.dockerjava.api.model.{Network => DNetwork}
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

  def getNetworkId(client: DockerClient, network: DockerNetwork): Try[String] =
    Try {
      client.inspectNetworkCmd().withNetworkId(network.name).exec().getId
    }

  def createOrGetNetwork(client: DockerClient, network: DockerNetwork): Try[DockerNetwork] =
    Try {
      client.listNetworksCmd().exec().exists((n: DNetwork) => n.getName == network.name) match {
        case true =>
          network
        case false =>
          client
            .createNetworkCmd()
            .withIpam(new DNetwork.Ipam().withConfig(new DNetwork.Ipam.Config().withSubnet(network.subnet)))
            .withName(network.name)
            .exec()
          network
      }
    }

  def createContainer[T](client: DockerClient, network: DockerNetwork, node: T)(implicit c: Containable[T]): Try[CreateContainerResponse] =
    Try {
      client
        .createContainerCmd(c.imageName)
        .withName(c.getContainerName(node))
        .withEnv(environmentMapToList(c.getEnvironment(node)))
        .withPortBindings(c.getPortBindings(node))
        .withNetworkMode(network.name)
        .withIpv4Address(c.getIpv4Address(node))
        .exec()
    }

  def startContainer(client: DockerClient, containerId: String): Try[Unit] =
    Try(client.startContainerCmd(containerId).exec())
}
