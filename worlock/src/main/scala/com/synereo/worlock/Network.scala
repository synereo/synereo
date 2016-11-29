package com.synereo.worlock

import java.net.InetSocketAddress

import com.biosimilarity.evaluator.distribution.DeploymentMode

trait Network {

  case class DockerNetwork(name: String, subnet: String)

  case class UsedContainer(name: String, stopped: Boolean, destroyed: Boolean)

  sealed trait Node {
    val name: String
    val deploymentMode: DeploymentMode
    val address: InetSocketAddress
    val dslCommLinkServer: Node
    val dslCommLinkClients: List[Node]
    val dslCommLinkClientsStageTwo: List[Node]
    val dslEvaluator: Node
    val dslEvaluatorStageTwo: Node
    val dslEvaluatorPreferredSupplier: Node
    val dslEvaluatorPreferredSupplierStageTwo: Node
    val bFactoryCommLinkServer: Node
    val bFactoryCommLinkClient: Node
    val bFactoryEvaluator: Node
    val exposedDebugPort: Option[Int]
    val exposedMongoPort: Option[Int]
    val exposedRabbitManagementPort: Option[Int]
    val suspendForDebugger: Boolean
  }

  final class HeadedNode(val name: String,
                         val deploymentMode: DeploymentMode,
                         val address: InetSocketAddress,
                         _dslCommLinkServer: => Node,
                         _dslCommLinkClients: => List[Node],
                         _dslCommLinkClientsStageTwo: => List[Node],
                         _dslEvaluator: => Node,
                         _dslEvaluatorStageTwo: => Node,
                         _dslEvaluatorPreferredSupplier: => Node,
                         _dslEvaluatorPreferredSupplierStageTwo: => Node,
                         _bFactoryCommLinkServer: => Node,
                         _bFactoryCommLinkClient: => Node,
                         _bFactoryEvaluator: => Node,
                         val serverPort: Int,
                         val exposedServerPort: Option[Int],
                         val serverSSLPort: Int,
                         val exposedServerSSLPort: Option[Int],
                         val exposedDebugPort: Option[Int],
                         val exposedMongoPort: Option[Int],
                         val exposedRabbitManagementPort: Option[Int],
                         val suspendForDebugger: Boolean)
      extends Node {

    lazy val dslCommLinkServer                     = _dslCommLinkServer
    lazy val dslCommLinkClients                    = _dslCommLinkClients
    lazy val dslCommLinkClientsStageTwo            = _dslCommLinkClientsStageTwo
    lazy val dslEvaluator                          = _dslEvaluator
    lazy val dslEvaluatorStageTwo                  = _dslEvaluatorStageTwo
    lazy val dslEvaluatorPreferredSupplier         = _dslEvaluatorPreferredSupplier
    lazy val dslEvaluatorPreferredSupplierStageTwo = _dslEvaluatorPreferredSupplierStageTwo
    lazy val bFactoryCommLinkServer                = _bFactoryCommLinkServer
    lazy val bFactoryCommLinkClient                = _bFactoryCommLinkClient
    lazy val bFactoryEvaluator                     = _bFactoryEvaluator
  }

  object HeadedNode {
    def apply(name: String,
              deploymentMode: DeploymentMode,
              address: InetSocketAddress,
              dslCommLinkServer: => Node,
              dslCommLinkClients: => List[Node],
              dslCommLinkClientsStageTwo: => List[Node],
              dslEvaluator: => Node,
              dslEvaluatorStageTwo: => Node,
              dslEvaluatorPreferredSupplier: => Node,
              dslEvaluatorPreferredSupplierStageTwo: => Node,
              bFactoryCommLinkServer: => Node,
              bFactoryCommLinkClient: => Node,
              bFactoryEvaluator: => Node,
              serverPort: Int,
              exposedServerPort: Option[Int],
              serverSSLPort: Int,
              exposedServerSSLPort: Option[Int],
              exposedDebugPort: Option[Int],
              exposedMongoPort: Option[Int],
              exposedRabbitManagementPort: Option[Int],
              suspendForDebugger: Boolean): HeadedNode =
      new HeadedNode(name,
                     deploymentMode,
                     address,
                     dslCommLinkServer,
                     dslCommLinkClients,
                     dslCommLinkClientsStageTwo,
                     dslEvaluator,
                     dslEvaluatorStageTwo,
                     dslEvaluatorPreferredSupplier,
                     dslEvaluatorPreferredSupplierStageTwo,
                     bFactoryCommLinkServer,
                     bFactoryCommLinkClient,
                     bFactoryEvaluator,
                     serverPort,
                     exposedServerPort,
                     serverSSLPort,
                     exposedServerSSLPort,
                     exposedDebugPort,
                     exposedMongoPort,
                     exposedRabbitManagementPort,
                     suspendForDebugger)
  }

  final class HeadlessNode(val name: String,
                           val deploymentMode: DeploymentMode,
                           val address: InetSocketAddress,
                           _dslCommLinkServer: => Node,
                           _dslCommLinkClients: => List[Node],
                           _dslCommLinkClientsStageTwo: => List[Node],
                           _dslEvaluator: => Node,
                           _dslEvaluatorStageTwo: => Node,
                           _dslEvaluatorPreferredSupplier: => Node,
                           _dslEvaluatorPreferredSupplierStageTwo: => Node,
                           _bFactoryCommLinkServer: => Node,
                           _bFactoryCommLinkClient: => Node,
                           _bFactoryEvaluator: => Node,
                           val exposedDebugPort: Option[Int],
                           val exposedMongoPort: Option[Int],
                           val exposedRabbitManagementPort: Option[Int],
                           val suspendForDebugger: Boolean)
      extends Node {

    lazy val dslCommLinkServer                     = _dslCommLinkServer
    lazy val dslCommLinkClients                    = _dslCommLinkClients
    lazy val dslCommLinkClientsStageTwo            = _dslCommLinkClientsStageTwo
    lazy val dslEvaluator                          = _dslEvaluator
    lazy val dslEvaluatorStageTwo                  = _dslEvaluator
    lazy val dslEvaluatorPreferredSupplier         = _dslEvaluatorPreferredSupplier
    lazy val dslEvaluatorPreferredSupplierStageTwo = _dslEvaluatorPreferredSupplierStageTwo
    lazy val bFactoryCommLinkServer                = _bFactoryCommLinkServer
    lazy val bFactoryCommLinkClient                = _bFactoryCommLinkClient
    lazy val bFactoryEvaluator                     = _bFactoryEvaluator
  }

  object HeadlessNode {
    def apply(name: String,
              deploymentMode: DeploymentMode,
              address: InetSocketAddress,
              dslCommLinkServer: => Node,
              dslCommLinkClients: => List[Node],
              dslCommLinkClientsStageTwo: => List[Node],
              dslEvaluator: => Node,
              dslEvaluatorStageTwo: => Node,
              dslEvaluatorPreferredSupplier: => Node,
              dslEvaluatorPreferredSupplierStageTwo: => Node,
              bFactoryCommLinkServer: => Node,
              bFactoryCommLinkClient: => Node,
              bFactoryEvaluator: => Node,
              exposedDebugPort: Option[Int],
              exposedMongoPort: Option[Int],
              exposedRabbitManagementPort: Option[Int],
              suspendForDebugger: Boolean): HeadlessNode =
      new HeadlessNode(name,
                       deploymentMode,
                       address,
                       dslCommLinkServer,
                       dslCommLinkClients,
                       dslCommLinkClientsStageTwo,
                       dslEvaluator,
                       dslEvaluatorStageTwo,
                       dslEvaluatorPreferredSupplier,
                       dslEvaluatorPreferredSupplierStageTwo,
                       bFactoryCommLinkServer,
                       bFactoryCommLinkClient,
                       bFactoryEvaluator,
                       exposedDebugPort,
                       exposedMongoPort,
                       exposedRabbitManagementPort,
                       suspendForDebugger)
  }
}
