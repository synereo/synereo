package com.synereo.worlock

import java.net.InetSocketAddress

import com.biosimilarity.evaluator.distribution.{Colocated, DeploymentMode}

trait Network {

  sealed trait Node {
    val name: String
    val deploymentMode: DeploymentMode
    val address: InetSocketAddress
    val dslCommLinkServer: Node
    val dslCommLinkClients: List[Node]
    val dslEvaluator: Node
    val dslEvaluatorPreferredSupplier: Node
    val bFactoryCommLinkServer: Node
    val bFactoryCommLinkClient: Node
    val bFactoryEvaluator: Node
    val rabbitPort: Int
    val exposedRabbitPort: Option[Int]
  }

  final class Headed(val name: String,
                     val deploymentMode: DeploymentMode,
                     val address: InetSocketAddress,
                     _dslCommLinkServer: => Node,
                     _dslCommLinkClients: => List[Node],
                     _dslEvaluator: => Node,
                     _dslEvaluatorPreferredSupplier: => Node,
                     _bFactoryCommLinkServer: => Node,
                     _bFactoryCommLinkClient: => Node,
                     _bFactoryEvaluator: => Node,
                     val rabbitPort: Int,
                     val exposedRabbitPort: Option[Int],
                     val serverPort: Int,
                     val exposedServerPort: Option[Int],
                     val serverSSLPort: Int,
                     val exposedServerSSLPort: Option[Int])
    extends Node {

    lazy val dslCommLinkServer             = _dslCommLinkServer
    lazy val dslCommLinkClients            = _dslCommLinkClients
    lazy val dslEvaluator                  = _dslEvaluator
    lazy val dslEvaluatorPreferredSupplier = _dslEvaluatorPreferredSupplier
    lazy val bFactoryCommLinkServer        = _bFactoryCommLinkServer
    lazy val bFactoryCommLinkClient        = _bFactoryCommLinkClient
    lazy val bFactoryEvaluator             = _bFactoryEvaluator
  }

  object Headed {
    def apply(name: String,
              deploymentMode: DeploymentMode,
              address: InetSocketAddress,
              dslCommLinkServer: => Node,
              dslCommLinkClients: => List[Node],
              dslEvaluator: => Node,
              dslEvaluatorPreferredSupplier: => Node,
              bFactoryCommLinkServer: => Node,
              bFactoryCommLinkClient: => Node,
              bFactoryEvaluator: => Node,
              rabbitPort: Int,
              exposedRabbitPort: Option[Int],
              serverPort: Int,
              exposedServerPort: Option[Int],
              serverSSLPort: Int,
              exposedServerSSLPort: Option[Int]): Headed =
      new Headed(
        name,
        deploymentMode,
        address,
        dslCommLinkServer,
        dslCommLinkClients,
        dslEvaluator,
        dslEvaluatorPreferredSupplier,
        bFactoryCommLinkServer,
        bFactoryCommLinkClient,
        bFactoryEvaluator,
        rabbitPort,
        exposedRabbitPort,
        serverPort,
        exposedServerPort,
        serverSSLPort,
        exposedServerSSLPort)
  }

  final class Headless(val name: String,
                       val deploymentMode: DeploymentMode,
                       val address: InetSocketAddress,
                       _dslCommLinkServer: => Node,
                       _dslCommLinkClients: => List[Node],
                       _dslEvaluator: => Node,
                       _dslEvaluatorPreferredSupplier: => Node,
                       _bFactoryCommLinkServer: => Node,
                       _bFactoryCommLinkClient: => Node,
                       _bFactoryEvaluator: => Node,
                       val rabbitPort: Int,
                       val exposedRabbitPort: Option[Int])
    extends Node {

    lazy val dslCommLinkServer             = _dslCommLinkServer
    lazy val dslCommLinkClients            = _dslCommLinkClients
    lazy val dslEvaluator                  = _dslEvaluator
    lazy val dslEvaluatorPreferredSupplier = _dslEvaluatorPreferredSupplier
    lazy val bFactoryCommLinkServer        = _bFactoryCommLinkServer
    lazy val bFactoryCommLinkClient        = _bFactoryCommLinkClient
    lazy val bFactoryEvaluator             = _bFactoryEvaluator
  }

  object Headless {
    def apply(name: String,
              deploymentMode: DeploymentMode,
              address: InetSocketAddress,
              dslCommLinkServer: => Node,
              dslCommLinkClients: => List[Node],
              dslEvaluator: => Node,
              dslEvaluatorPreferredSupplier: => Node,
              bFactoryCommLinkServer: => Node,
              bFactoryCommLinkClient: => Node,
              bFactoryEvaluator: => Node,
              rabbitPort: Int,
              exposedRabbitPort: Option[Int]): Headless =
      new Headless(
        name,
        deploymentMode,
        address,
        dslCommLinkServer,
        dslCommLinkClients,
        dslEvaluator,
        dslEvaluatorPreferredSupplier,
        bFactoryCommLinkServer,
        bFactoryCommLinkClient,
        bFactoryEvaluator,
        rabbitPort,
        exposedRabbitPort)
  }
}
