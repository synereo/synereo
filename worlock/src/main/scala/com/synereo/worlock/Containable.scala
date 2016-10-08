package com.synereo.worlock

import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.distribution.{Colocated, Distributed}
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model.{ExposedPort, Ports}

trait Containable[T] {

  val imageName: String

  def getContainerName(a: T): String

  def getEnvironment(a: T): Map[String, String]

  def getPortBindings(a: T): Ports
}

object Containable {

  implicit object containableNode extends Containable[Node] {

    val imageName: String = s"gloseval:${BuildInfo.version}"

    def getContainerName(n: Node): String = n.name

    def getEnvironment(n: Node): Map[String, String] =
      Map[String, String](
        "DEPLOYMENT_MODE" ->
          n.deploymentMode.toString,
        "DSL_COMM_LINK_CLIENT_HOSTS" ->
          n.dslCommLinkClients.foldLeft(List.empty[String]) { (accum: List[String], node: Node) =>
            node.address.getAddress.toString.substring(1) + ":" + node.address.getPort.toString :: accum
          }.mkString(","),
        "DSL_EVALUATOR_HOST" ->
          n.dslEvaluator.address.getAddress.toString.substring(1),
        "DSL_EVALUATOR_PORT" ->
          n.dslEvaluator.address.getPort.toString,
        "DSL_EVALUATOR_PREFERRED_SUPPLIER_HOST" ->
          n.dslEvaluatorPreferredSupplier.address.getAddress.toString.substring(1),
        "DSL_EVALUATOR_PREFERRED_SUPPLIER_PORT" ->
          n.dslEvaluatorPreferredSupplier.address.getPort.toString,
        "BFACTORY_COMM_LINK_SERVER_HOST" ->
          n.bFactoryCommLinkServer.address.getAddress.toString.substring(1),
        "BFACTORY_COMM_LINK_SERVER_PORT" ->
          n.bFactoryCommLinkClient.address.getPort.toString,
        "BFACTORY_COMM_LINK_CLIENT_HOST" ->
          n.bFactoryCommLinkClient.address.getAddress.toString.substring(1),
        "BFACTORY_COMM_LINK_CLIENT_PORT" ->
          n.bFactoryCommLinkClient.address.getPort.toString,
        "BFACTORY_EVALUATOR_HOST" ->
          n.bFactoryEvaluator.address.getAddress.toString.substring(1),
        "BFACTORY_EVALUATOR_PORT" ->
          n.bFactoryEvaluator.address.getPort.toString)

    private def createPortBindings(portMap: Map[Int, Option[Int]]): Ports = {
      val ports: Ports = new Ports()
      portMap.foreach {
        case (inner, Some(exposed)) =>
          ports.bind(ExposedPort.tcp(exposed), Binding.bindPort(inner))
        case _ =>
      }
      ports
    }

    def getPortBindings(n: Node): Ports = n match {
      case x: Headed if x.deploymentMode == Colocated =>
        createPortBindings(
          Map(
            x.serverPort -> x.exposedServerPort,
            x.serverSSLPort -> x.exposedServerSSLPort))
      case x: Headed if x.deploymentMode == Distributed =>
        createPortBindings(
          Map(
            x.rabbitPort -> x.exposedRabbitPort,
            x.serverPort -> x.exposedServerPort,
            x.serverSSLPort -> x.exposedServerSSLPort))
      case x: Headless if x.deploymentMode == Colocated =>
        createPortBindings(
          Map(
            x.rabbitPort -> x.exposedRabbitPort))
      case x: Headless if x.deploymentMode == Distributed =>
        createPortBindings(
          Map(
            x.rabbitPort -> x.exposedRabbitPort))
    }
  }
}
