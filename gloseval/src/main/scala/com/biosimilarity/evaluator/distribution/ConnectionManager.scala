package com.biosimilarity.evaluator.distribution

import com.biosimilarity.lift.lib.BasicLogService
import com.biosimilarity.evaluator.spray.EvalHandlerService
import com.biosimilarity.lift.model.store.CnxnCtxtLabel

import scala.util.continuations._

object ConnectionManager {

  import EvalHandlerService._
  import ConcreteHL._
  import DSLCommLink._

  type Rsrc = mTT.Resource

  val agentManager: AgentManager = agentMgr()

  def sendStartEngineRequest(erql: CnxnCtxtLabel[String, String, String],
                             erspl: CnxnCtxtLabel[String, String, String],
                             onReceive: Option[mTT.Resource] => Unit): Unit = {
    reset {
      node().publish(erql, StartEngineRequest("stage-two.conf"))
    }
    reset {
      try {
        node().subscribe(erspl).foreach { (optRsrc: Option[mTT.Resource]) =>
          onReceive(optRsrc)
        }
      } catch {
        case e: Throwable =>
          BasicLogService.tweetTrace(e.asInstanceOf[Exception])
          throw e
      }
    }
  }

  def sendStartEngineRequest(): Unit = {
    val (erql, erspl) = agentManager.makePolarizedAdminPair()
    def handler(optRsrc: Option[mTT.Resource]): Unit = optRsrc.foreach { (rsrc: mTT.Resource) =>
      BasicLogService.tweet(s"Hello from the NEUER REALITY: $rsrc")
    }
    sendStartEngineRequest(erql, erspl, handler)
  }
}
