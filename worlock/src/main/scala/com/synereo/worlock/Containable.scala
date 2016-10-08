package com.synereo.worlock

import com.biosimilarity.evaluator.distribution.{Colocated, Distributed}
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model.{ExposedPort, Ports}

trait Containable[T] {

  def createEnvironment(a: T): Map[String, String]

  def getPortBindings(a: T): Ports
}

object Containable {

  implicit object nodeContainer extends Containable[Node] {

    def createEnvironment(n: Node): Map[String, String] =
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
          n.bFactoryEvaluator.address.getPort.toString
      )

    private def createPortBindings(portMap: Map[Option[Int], Int]): Ports = {
      val ports: Ports = new Ports()
      portMap.foreach {
        case (Some(exposed), inner) =>
          ports.bind(ExposedPort.tcp(exposed), Binding.bindPort(inner))
        case _ =>
      }
      ports
    }

    def getPortBindings(n: Node): Ports = n match {
      case x: Headed if x.deploymentMode == Colocated =>
        createPortBindings(
          Map(
            x.exposedServerPort -> x.serverPort,
            x.exposedServerSSLPort -> x.serverSSLPort))
      case x: Headed if x.deploymentMode == Distributed =>
        createPortBindings(
          Map(
            x.exposedRabbitPort -> x.rabbitPort,
            x.exposedServerPort -> x.serverPort,
            x.exposedServerSSLPort -> x.serverSSLPort))
      case x: Headless if x.deploymentMode == Colocated =>
        createPortBindings(
          Map(
            x.exposedRabbitPort -> x.rabbitPort))
      case x: Headless if x.deploymentMode == Distributed =>
        createPortBindings(
          Map(
            x.exposedRabbitPort -> x.rabbitPort))
    }
  }

}
