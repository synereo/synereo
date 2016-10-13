package com.synereo.worlock

import java.net.InetSocketAddress

import com.biosimilarity.evaluator.distribution.DeploymentMode

trait Network {

  case class DockerNetwork(name: String, subnet: String)

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
  }

  final class HeadedNode(val name: String,
                         val deploymentMode: DeploymentMode,
                         val address: InetSocketAddress,
                         _dslCommLinkServer: => Node,
                         _dslCommLinkClients: => List[Node],
                         _dslEvaluator: => Node,
                         _dslEvaluatorPreferredSupplier: => Node,
                         _bFactoryCommLinkServer: => Node,
                         _bFactoryCommLinkClient: => Node,
                         _bFactoryEvaluator: => Node,
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

  object HeadedNode {
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
              serverPort: Int,
              exposedServerPort: Option[Int],
              serverSSLPort: Int,
              exposedServerSSLPort: Option[Int]): HeadedNode =
      new HeadedNode(name,
                     deploymentMode,
                     address,
                     dslCommLinkServer,
                     dslCommLinkClients,
                     dslEvaluator,
                     dslEvaluatorPreferredSupplier,
                     bFactoryCommLinkServer,
                     bFactoryCommLinkClient,
                     bFactoryEvaluator,
                     serverPort,
                     exposedServerPort,
                     serverSSLPort,
                     exposedServerSSLPort)
  }

  final class HeadlessNode(val name: String,
                           val deploymentMode: DeploymentMode,
                           val address: InetSocketAddress,
                           _dslCommLinkServer: => Node,
                           _dslCommLinkClients: => List[Node],
                           _dslEvaluator: => Node,
                           _dslEvaluatorPreferredSupplier: => Node,
                           _bFactoryCommLinkServer: => Node,
                           _bFactoryCommLinkClient: => Node,
                           _bFactoryEvaluator: => Node)
      extends Node {

    lazy val dslCommLinkServer             = _dslCommLinkServer
    lazy val dslCommLinkClients            = _dslCommLinkClients
    lazy val dslEvaluator                  = _dslEvaluator
    lazy val dslEvaluatorPreferredSupplier = _dslEvaluatorPreferredSupplier
    lazy val bFactoryCommLinkServer        = _bFactoryCommLinkServer
    lazy val bFactoryCommLinkClient        = _bFactoryCommLinkClient
    lazy val bFactoryEvaluator             = _bFactoryEvaluator
  }

  object HeadlessNode {
    def apply(name: String,
              deploymentMode: DeploymentMode,
              address: InetSocketAddress,
              dslCommLinkServer: => Node,
              dslCommLinkClients: => List[Node],
              dslEvaluator: => Node,
              dslEvaluatorPreferredSupplier: => Node,
              bFactoryCommLinkServer: => Node,
              bFactoryCommLinkClient: => Node,
              bFactoryEvaluator: => Node): HeadlessNode =
      new HeadlessNode(name,
                       deploymentMode,
                       address,
                       dslCommLinkServer,
                       dslCommLinkClients,
                       dslEvaluator,
                       dslEvaluatorPreferredSupplier,
                       bFactoryCommLinkServer,
                       bFactoryCommLinkClient,
                       bFactoryEvaluator)
  }
}
