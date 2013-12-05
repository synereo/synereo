package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.{PortableAgentCnxn, PortableAgentBiCnxn}
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

trait IntroductionRecipientT extends Serializable {
  private val biCnxnsListLabel = "biCnxnsList(true)".toLabel
  private val profileDataLabel = "jsonBlob(W)".toLabel

  def run(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    if (cnxns.size != 2) throw new Exception("invalid number of cnxns supplied")

    val protocolMgr = new ProtocolManager(node)
    val readCnxn = cnxns(0)
    val aliasCnxn = cnxns(1)

    listenGetIntroductionProfileRequest(protocolMgr, readCnxn, aliasCnxn)
    listenIntroductionRequest(protocolMgr, readCnxn, aliasCnxn)
  }

  private def listenGetIntroductionProfileRequest(protocolMgr: ProtocolManager, readCnxn: PortableAgentCnxn, aliasCnxn: PortableAgentCnxn): Unit = {
    // listen for GetIntroductionProfileRequest message
    val getIntroProfileRqLabel = GetIntroductionProfileRequest.toLabel()
    protocolMgr.subscribeMessage(readCnxn, getIntroProfileRqLabel, {
      case GetIntroductionProfileRequest(sessionId, correlationId, rspCnxn) =>
        // TODO: get data from aliasCnxn once it is being stored there
        val identityCnxn = PortableAgentCnxn(aliasCnxn.src, "identity", aliasCnxn.trgt)

        // get the profile data
        protocolMgr.read(identityCnxn, profileDataLabel, {
          case Some(mTT.Ground(PostedExpr(jsonBlob: String))) =>
            val getIntroProfileRsp = GetIntroductionProfileResponse(sessionId, correlationId, jsonBlob)

            // send GetIntroductionProfileResponse message
            protocolMgr.putMessage(rspCnxn, getIntroProfileRsp)
          case _ => throw new Exception("unexpected profile data")
        })
    })
  }

  private def listenIntroductionRequest(protocolMgr: ProtocolManager, readCnxn: PortableAgentCnxn, aliasCnxn: PortableAgentCnxn): Unit = {
    // listen for IntroductionRequest message
    val introRqLabel = IntroductionRequest.toLabel()
    protocolMgr.subscribeMessage(readCnxn, introRqLabel, {
      case IntroductionRequest(sessionId, correlationId, rspCnxn, message, profileData) =>
        val introN = IntroductionNotification(sessionId, UUID.randomUUID.toString, PortableAgentBiCnxn(readCnxn, rspCnxn), message, profileData)

        // send IntroductionNotification message
        protocolMgr.publishMessage(aliasCnxn, introN)

        // listen for IntroductionConfirmation message
        val introConfirmationLabel = IntroductionConfirmation.toLabel(sessionId, introN.correlationId)
        protocolMgr.getMessage(aliasCnxn, introConfirmationLabel, {
          case IntroductionConfirmation(_, _, true) =>
            val introRsp = IntroductionResponse(sessionId, correlationId, true, Some(UUID.randomUUID.toString))

            // send IntroductionResponse message
            protocolMgr.putMessage(rspCnxn, introRsp)

            // listen for Connect message
            val connectLabel = Connect.toLabel(sessionId, introRsp.connectCorrelationId.get)
            protocolMgr.getMessage(readCnxn, connectLabel, {
              case Connect(_, _, false, Some(newBiCnxn)) =>
                // get the list of biCnxns
                protocolMgr.get(aliasCnxn, biCnxnsListLabel, {
                  case Some(mTT.Ground(PostedExpr(prevBiCnxns: String))) =>
                    // add new biCnxn to the list
                    val newBiCnxns = newBiCnxn :: Serializer.deserialize[List[PortableAgentBiCnxn]](prevBiCnxns)

                    // serialize biCnxn list
                    val newBiCnxnsStr = Serializer.serialize(newBiCnxns)

                    // save new list of biCnxns
                    protocolMgr.put(aliasCnxn, biCnxnsListLabel, mTT.Ground(PostedExpr(newBiCnxnsStr)))

                    val connectN = ConnectNotification(sessionId, newBiCnxn, profileData)

                    // send ConnectNotification message
                    protocolMgr.publishMessage(aliasCnxn, connectN)
                  case _ => throw new Exception("unexpected biCnxnsList data")
                })
              case Connect(_, _, true, None) => ()
            })
          case IntroductionConfirmation(_, _, false) =>
            val introRsp = IntroductionResponse(sessionId, correlationId, false, None)

            // send IntroductionResponse message
            protocolMgr.putMessage(rspCnxn, introRsp)
        })
    })
  }
}

class IntroductionRecipient extends IntroductionRecipientT {
  override def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    super.run(kvdbNode, cnxns, filters)
  }
}
