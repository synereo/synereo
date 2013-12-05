package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs.ProtocolMessage
import scala.util.continuations._

class ProtocolManager(node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]) {
  private def toAgentCnxn(cnxn: PortableAgentCnxn): acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }

  def putMessage(
    cnxn: PortableAgentCnxn,
    message: ProtocolMessage
  ): Unit = {
    //TODO: Remove this once race condition is resolved
    Thread.sleep(1000)
    reset {
      val agentCnxn = toAgentCnxn(cnxn)
      val filter = message.toLabel

      node.put(agentCnxn)(filter, mTT.Ground(PostedExpr(message)))
    }
  }

  def publishMessage(
    cnxn: PortableAgentCnxn,
    message: ProtocolMessage
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)
      val filter = message.toLabel

      node.publish(agentCnxn)(filter, mTT.Ground(PostedExpr(message)))
    }
  }

  def getMessage(
    cnxn: PortableAgentCnxn,
    filter: CnxnCtxtLabel[String, String, String],
    onResult: ProtocolMessage => Unit
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)

      for (e <- node.get(agentCnxn)(filter)) {
        e match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(message: ProtocolMessage))), _)) =>
            onResult(message)
          case None =>
          case _ => throw new Exception("Unexpected protocol message: " + e)
        }
      }
    }
  }

  def subscribeMessage(
    cnxn: PortableAgentCnxn,
    filter: CnxnCtxtLabel[String, String, String],
    onResult: ProtocolMessage => Unit
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)

      for (e <- node.subscribe(agentCnxn)(filter)) {
        e match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(message: ProtocolMessage))), _)) =>
            onResult(message)
          case None =>
          case _ => throw new Exception("Unexpected protocol message: " + e)
        }
      }
    }
  }

  def put(
    cnxn: PortableAgentCnxn,
    filter: CnxnCtxtLabel[String, String, String],
    value: mTT.Resource
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)

      node.put(agentCnxn)(filter, value)
    }
  }

  def get(
    cnxn: PortableAgentCnxn,
    filter: CnxnCtxtLabel[String, String, String],
    onResult: Option[mTT.Resource] => Unit
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)

      for (e <- node.get(agentCnxn)(filter)) { onResult(e) }
    }
  }

  def read(
    cnxn: PortableAgentCnxn,
    filter: CnxnCtxtLabel[String, String, String],
    onResult: Option[mTT.Resource] => Unit
  ): Unit = {
    reset {
      val agentCnxn = toAgentCnxn(cnxn)

      for (e <- node.read(agentCnxn)(filter)) { onResult(e) }
    }
  }
}
