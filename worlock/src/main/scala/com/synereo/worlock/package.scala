package com.synereo

import com.biosimilarity.evaluator.util._
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateContainerResponse
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

  def createContainer[T](
      client: DockerClient,
      network: DockerNetwork,
      node: T,
      additionalEnv: Map[String, String] = Map.empty[String, String])(implicit c: Containable[T]): Try[CreateContainerResponse] =
    Try {
      client
        .createContainerCmd(c.imageName)
        .withName(c.getContainerName(node))
        .withEnv(environmentMapToList(c.getEnvironment(node) ++ additionalEnv))
        .withPortBindings(c.getPortBindings(node))
        .withNetworkMode(network.name)
        .withIpv4Address(c.getIpv4Address(node))
        .exec()
    }

  def startContainer(client: DockerClient, containerId: String): Try[Unit] =
    Try(client.startContainerCmd(containerId).exec())

  /**
    * Create and start containers for a List of [[Node]]s
    * @param client an instance of [[com.github.dockerjava.api.DockerClient]]
    * @param network network on which to create the containers
    * @param nodes a List of node definitions
    * @param additionalEnvironment a Map containing additional environment variables to supply to the containers
    * @return
    */
  def setupContainers(client: DockerClient,
                      network: DockerNetwork,
                      nodes: List[Node],
                      additionalEnvironment: Map[String, String]): Try[List[CreateContainerResponse]] =
    nodes.map { (node: Node) =>
      createContainer(client, network, node, additionalEnvironment)
    }.traverse { (response: CreateContainerResponse) =>
      Thread.sleep(15000L)
      startContainer(client, response.getId).map((_: Unit) => response)
    }

  /**
    * Stops containers and conditionally destroys them
    * @param client an instance of [[com.github.dockerjava.api.DockerClient]]
    * @param containers a List of containers
    * @param destroy if true, then destroy containers after stopping them, otherwise save them
    * @return
    */
  def teardownContainers(client: DockerClient,
                         containers: List[CreateContainerResponse],
                         destroy: Boolean): Try[List[UsedContainer]] =
    containers.map { (container: CreateContainerResponse) =>
      for {
        name      <- Try(client.inspectContainerCmd(container.getId).exec().getName.substring(1))
        stopped   <- Try(client.stopContainerCmd(container.getId).exec()).map((_: Void) => true)
        destroyed <- Try(client.removeContainerCmd(container.getId).exec()).map((_: Void) => true) if destroy
      } yield UsedContainer(name, stopped, destroyed)
    }.sequence
}
