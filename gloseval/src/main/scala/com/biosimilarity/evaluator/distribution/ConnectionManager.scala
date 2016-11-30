package com.biosimilarity.evaluator.distribution

import java.net.{InetSocketAddress, URI}

import com.biosimilarity.evaluator.spray.EvalHandlerService
import com.biosimilarity.lift.lib.uri._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel

import scala.util.continuations._

object ConnectionManager {

  import ConcreteHL._
  import DSLCommLink._
  import EvalHandlerService._

  type Rsrc = mTT.Resource

  val agentManager: AgentManager = agentMgr()

  def sendConnectToClientRequest(erql: CnxnCtxtLabel[String, String, String],
                                 erspl: CnxnCtxtLabel[String, String, String],
                                 localhost: URI,
                                 uris: Seq[URI],
                                 onReceive: (Option[mTT.Resource]) => Unit): Unit = {
    reset {
      node().publish(erql, ConnectToClientRequest("stage-two.conf", localhost, uris))
    }
    reset {
      node().subscribe(erspl).foreach((optRsrc: Option[mTT.Resource]) => onReceive(optRsrc))
    }
  }

  def sendConnectToClientRequest(localhost: URI, uris: Seq[URI]): Unit = {
    val (erql, erspl) = agentManager.makePolarizedAdminPair()
    def handler(optRsrc: Option[mTT.Resource]): Unit = optRsrc.foreach { (rsrc: mTT.Resource) =>
      println(s"Hello from the NEUER REALITY: ${rsrc.toString}")
    }
    sendConnectToClientRequest(erql, erspl, localhost, uris, handler)
    println("We made the call to sendStartEngineRequest")
  }

  def pureConvenienceMethod(): Unit = {
    val dslEvaluatorHostData: String = EvalConfigWrapper.readString("DSLEvaluatorHostData")
    val dslEvaluatorHostName: String = EvalConfigWrapper.readString("DSLEvaluatorHost")
    val dslEvaluatorHostPort: Int    = EvalConfigWrapper.readInt("DSLEvaluatorPort")
    val localhost: URI               = new URI("agent", null, dslEvaluatorHostName, dslEvaluatorHostPort, dslEvaluatorHostData, null, null)
    val uris: Seq[URI] = evalConfig("stage-two.conf").getString("DSLCommLinkClientHosts").split(',').toList.map { (s: String) =>
      val inetSocketAddress: InetSocketAddress = parseInetSocketAddress(s, "localhost", 5672)
      new URI("agent", null, inetSocketAddress.getHostString, inetSocketAddress.getPort, dslEvaluatorHostData, null, null)
    }
    ConnectionManager.sendConnectToClientRequest(localhost, uris)
  }
}
