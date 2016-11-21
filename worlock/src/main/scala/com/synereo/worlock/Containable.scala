package com.synereo.worlock

import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.distribution.{Colocated, Distributed}
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model.{ExposedPort, Ports}

trait Containable[T] {

  val imageName: String

  def getContainerName(a: T): String

  def getIpv4Address(a: T): String

  def getEnvironment(a: T): Map[String, String]

  def getPortBindings(a: T): Ports
}

object Containable {

  implicit object containableNode extends Containable[Node] {

    val imageName: String = s"synereo/synereo-node:${BuildInfo.version}"

    private val internalJVMDebugPort: Int         = 5005
    private val internalMongoPort: Int            = 27017
    private val internalRabbitManagementPort: Int = 55672

    def getContainerName(n: Node): String = n.name

    def getIpv4Address(n: Node): String = n.address.getAddress.toString.substring(1)

    def getEnvironment(n: Node): Map[String, String] =
      Map[String, String](
        "JAVA_OPTS" -> {
          val suspend = if (n.suspendForDebugger) "y" else "n"
          s"-agentlib:jdwp=transport=dt_socket,server=y,suspend=$suspend,address=$internalJVMDebugPort"
        },
        "DEPLOYMENT_MODE" ->
          n.deploymentMode.toString,
        "DSL_COMM_LINK_SERVER_HOST" ->
          n.dslCommLinkServer.address.getHostString,
        "DSL_COMM_LINK_SERVER_PORT" ->
          n.dslCommLinkServer.address.getPort.toString,
        "DSL_COMM_LINK_CLIENT_HOSTS" ->
          n.dslCommLinkClients
            .foldLeft(List.empty[String]) { (accum: List[String], node: Node) =>
              node.address.getHostString + ":" + node.address.getPort.toString :: accum
            }
            .mkString(","),
        "DSL_EVALUATOR_HOST" ->
          n.dslEvaluator.address.getHostString,
        "DSL_EVALUATOR_PORT" ->
          n.dslEvaluator.address.getPort.toString,
        "DSL_EVALUATOR_PREFERRED_SUPPLIER_HOST" ->
          n.dslEvaluatorPreferredSupplier.address.getHostString,
        "DSL_EVALUATOR_PREFERRED_SUPPLIER_PORT" ->
          n.dslEvaluatorPreferredSupplier.address.getPort.toString,
        "BFACTORY_COMM_LINK_SERVER_HOST" ->
          n.bFactoryCommLinkServer.address.getHostString,
        "BFACTORY_COMM_LINK_SERVER_PORT" ->
          n.bFactoryCommLinkClient.address.getPort.toString,
        "BFACTORY_COMM_LINK_CLIENT_HOST" ->
          n.bFactoryCommLinkClient.address.getHostString,
        "BFACTORY_COMM_LINK_CLIENT_PORT" ->
          n.bFactoryCommLinkClient.address.getPort.toString,
        "BFACTORY_EVALUATOR_HOST" ->
          n.bFactoryEvaluator.address.getHostString,
        "BFACTORY_EVALUATOR_PORT" ->
          n.bFactoryEvaluator.address.getPort.toString
      ) ++ (n match {
        case headed: HeadedNode =>
          Map[String, String]("MODE" ->
                                "headed",
                              "SERVER_PORT" ->
                                headed.serverPort.toString,
                              "SERVER_SSL_PORT" ->
                                headed.serverSSLPort.toString)
        case headless: HeadlessNode =>
          Map[String, String](
            "MODE" ->
              "headless")
      })

    private def createPortBindings(portMap: Map[Int, Option[Int]]): Ports = {
      val ports: Ports = new Ports()
      portMap.foreach {
        case (inner, Some(exposed)) =>
          ports.bind(ExposedPort.tcp(inner), Binding.bindPort(exposed))
        case (_, None) =>
      }
      ports
    }

    def getPortBindings(n: Node): Ports = n match {
      case a: HeadedNode if a.deploymentMode == Colocated =>
        createPortBindings(
          Map(a.serverPort                 -> a.exposedServerPort,
              a.serverSSLPort              -> a.exposedServerSSLPort,
              internalJVMDebugPort         -> a.exposedDebugPort,
              internalMongoPort            -> a.exposedMongoPort,
              internalRabbitManagementPort -> a.exposedRabbitManagementPort))
      case b: HeadedNode if b.deploymentMode == Distributed =>
        createPortBindings(
          Map(b.serverPort                 -> b.exposedServerPort,
              b.serverSSLPort              -> b.exposedServerSSLPort,
              internalJVMDebugPort         -> b.exposedDebugPort,
              internalMongoPort            -> b.exposedMongoPort,
              internalRabbitManagementPort -> b.exposedRabbitManagementPort))
      case c: HeadlessNode if c.deploymentMode == Colocated =>
        createPortBindings(
          Map(internalJVMDebugPort         -> c.exposedDebugPort,
              internalMongoPort            -> c.exposedMongoPort,
              internalRabbitManagementPort -> c.exposedRabbitManagementPort))
      case d: HeadlessNode if d.deploymentMode == Distributed =>
        createPortBindings(
          Map(internalJVMDebugPort         -> d.exposedDebugPort,
              internalMongoPort            -> d.exposedMongoPort,
              internalRabbitManagementPort -> d.exposedRabbitManagementPort))
    }
  }
}
