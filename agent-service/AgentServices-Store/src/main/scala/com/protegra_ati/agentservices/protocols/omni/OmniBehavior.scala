package com.protegra_ati.agentservices.protocols.omni

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.ProtocolBehaviorT
import com.protegra_ati.agentservices.protocols.omni.msgs._
import com.synereo.wallet.models.{AMPKey, BalanceSummary}

import scala.util.continuations._

trait OmniBehaviorT extends ProtocolBehaviorT with Serializable {

  import com.biosimilarity.evaluator.distribution.utilities.DieselValueTrampoline._

  def run(
     node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
     cnxns: Seq[PortableAgentCnxn],
     filters: Seq[CnxnCtxtLabel[String, String, String]]
   ): Unit = {
    cnxns match {
      case Seq(readCnxn, writeCnxn) => listenOmniCall(node, readCnxn, writeCnxn)
      case _ => throw new Exception(s"two cnxns expected : $cnxns")
    }
  }

  def listenOmniCall(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    readCnxn: PortableAgentCnxn,
    writeCnxn: PortableAgentCnxn
  ): Unit = {
    // transform connections
    val agentReadCnxn =
      acT.AgentCnxn(readCnxn.src, readCnxn.label, readCnxn.trgt)
    val agentWriteCnxn =
      acT.AgentCnxn(writeCnxn.src, writeCnxn.label, writeCnxn.trgt)

    reset {
      // subscribe balances
      for (r <- node.subscribe(agentReadCnxn)(BalanceRequestMessage.toLabel)) {
        rsrc2V[OmniMessage](r) match {
          case Left(BalanceRequestMessage(sid, cid, user, priv)) =>
            val bs: BalanceSummary = AMPKey(user, priv).balance
            val rsp = BalanceResponseMessage(sid, cid, bs.address, bs.amp, bs.btc, bs.errors.mkString(";"))
            node.put(agentWriteCnxn)(
              BalanceResponseMessage.toLabel(sid),
              rsp
            )
          case Right(true) =>
            println(s"Waiting for balance message")

          case _ =>
            println(s"Not an omni protocol message $r")

        }
      }
    }
    reset {
      // subscribe tranfer
      for (r <- node.subscribe(agentReadCnxn)(SendAmpsRequestMessage.toLabel)) {
        rsrc2V[OmniMessage](r) match {
          case Left(SendAmpsRequestMessage(sid, cid, userFrom, privFrom, userTo, privTo, amount)) =>
            val transaction = AMPKey(userFrom, privFrom).transfer(AMPKey(userTo, privTo), amount)
            val rsp = SendAmpsResponseMessage(sid, cid, userFrom, userTo, transaction)
            node.put(agentWriteCnxn)(
              SendAmpsResponseMessage.toLabel(sid),
              rsp
            )
          case Right(true) =>
            println(s"Waiting for transfer message")

          case _ =>
            println(s"Not an omni protocol message $r")

        }
      }
    }
  }
}

class OmniBehavior extends OmniBehaviorT {
  override def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    super.run(kvdbNode, cnxns, filters)
  }
}

object OmniBehavior {
  def apply( ) : OmniBehavior = new OmniBehavior()
  def unapply( ob : OmniBehavior ) = Some( () )
}