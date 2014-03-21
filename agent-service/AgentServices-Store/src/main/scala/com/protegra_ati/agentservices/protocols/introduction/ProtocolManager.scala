package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs.ProtocolMessage
import scala.util.continuations._

class ProtocolManager(val node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]) extends Serializable {
  def this() = { this( null.asInstanceOf[Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]] ) }
  private def toAgentCnxn(cnxn: PortableAgentCnxn): acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }

  def putMessage(
    cnxn: PortableAgentCnxn,
    message: ProtocolMessage
  ): Unit = {
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
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nProtocolMgr -- getMessage | get returned " 
            + "\nnode: " + node
            + "\ncnxn: " + cnxn
            + "\nfilter: " + filter
            + "\ne: " + e
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        e match {
          // colocated 
          case Some(mTT.Ground(PostedExpr(message: ProtocolMessage))) =>
            onResult(message)
          // distributed
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
        // println(
//           (
//             "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
//             + "\nProtocolMgr -- subscribeMessage | subscribe returned " 
//             + "\nnode: " + node
//             + "\ncnxn: " + cnxn
//             + "\nfilter: " + filter
//             + "\ne: " + e
//             + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
//           )
//         )
        e match {
          // colocated 
          case Some(mTT.Ground(PostedExpr(message: ProtocolMessage))) =>
            onResult(message)
          // distributed
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

      for (e <- node.get(agentCnxn)(filter)) {
        // println(
//           (
//             "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
//             + "\nProtocolMgr -- get | get returned " 
//             + "\nnode: " + node
//             + "\ncnxn: " + cnxn
//             + "\nfilter: " + filter
//             + "\ne: " + e
//             + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
//           )
//         )
        onResult(e)
      }
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

package usage {
  import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
  import com.protegra_ati.agentservices.protocols.msgs._
  import java.net.URI

  object ProtocolManagerHarness extends Serializable {
    import java.util.UUID
    def reproLabel() : Option[CnxnCtxtLabel[String,String,String]] = {
      Some( BeginIntroductionRequest.toLabel() )
    }
    def reproCnxn() : Option[PortableAgentCnxn] = {
      Some(
        PortableAgentCnxn(
          new URI("agent://e6eca9b84dd49d91e45c9e23cd74d2d98bb1"),
          "alias",
          new URI("agent://e6eca9b84dd49d91e45c9e23cd74d2d98bb1")
        )
      )
    }
    def reproNode() : String = {
      val dslNodeId = UUID.randomUUID()
      val dslNodeKey = dslNodeId.toString      
      com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper += ( dslNodeKey -> com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.dslEvaluatorAgent( ) )
      dslNodeKey
    }
    def protocolMgr( nodeKey : String ) : Option[ProtocolManager] = {
      for(
        node <- com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper.get( nodeKey ) ) yield {
          new ProtocolManager( node ) 
        }
    }
    def repro() = {
      val rpnKey = reproNode()
      for(
        pm <- protocolMgr( rpnKey );
        label <- reproLabel();
        cnxn <- reproCnxn()
      ) {
        pm.subscribeMessage( cnxn, label, { e : ProtocolMessage => println( e ) } )
      }
    }
  }
}
