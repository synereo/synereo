package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.{PortableAgentCnxn, PortableAgentBiCnxn}
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import java.util.UUID

trait IntroductionInitiatorT extends Serializable {
  def run(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    if (cnxns.size != 1) throw new Exception("invalid number of cnxns supplied")

    val protocolMgr = new ProtocolManager(node)
    val aliasCnxn = cnxns(0)

    listenBeginIntroductionRequest(protocolMgr, aliasCnxn)
  }

  private def listenBeginIntroductionRequest(protocolMgr: ProtocolManager, aliasCnxn: PortableAgentCnxn): Unit = {
    // listen for BeginIntroductionRequest message
    val beginIntroRqLabel = BeginIntroductionRequest.toLabel()
    protocolMgr.subscribeMessage(aliasCnxn, beginIntroRqLabel, {
      case BeginIntroductionRequest(sessionId, PortableAgentBiCnxn(aReadCnxn, aWriteCnxn), PortableAgentBiCnxn(bReadCnxn, bWriteCnxn), aMessage, bMessage) =>
        val aGetIntroProfileRq = GetIntroductionProfileRequest(sessionId, UUID.randomUUID.toString, aReadCnxn)

        // send A's GetIntroductionProfileRequest message
        protocolMgr.publishMessage(aWriteCnxn, aGetIntroProfileRq)

        // listen for A's GetIntroductionProfileResponse message
        val aGetIntroProfileRspLabel = GetIntroductionProfileResponse.toLabel(sessionId, aGetIntroProfileRq.correlationId)
        protocolMgr.getMessage(aReadCnxn, aGetIntroProfileRspLabel, {
          case GetIntroductionProfileResponse(_, _, aProfileData) =>
            val bGetIntroProfileRq = GetIntroductionProfileRequest(sessionId, UUID.randomUUID.toString, bReadCnxn)

            // send B's GetIntroductionProfileRequest message
            protocolMgr.publishMessage(bWriteCnxn, bGetIntroProfileRq)

            // listen for B's GetIntroductionProfileResponse message
            val bGetIntroProfileRspLabel = GetIntroductionProfileResponse.toLabel(sessionId, bGetIntroProfileRq.correlationId)
            protocolMgr.getMessage(bReadCnxn, bGetIntroProfileRspLabel, {
              case GetIntroductionProfileResponse(_, _, bProfileData) =>
                val aIntroRq = IntroductionRequest(sessionId, UUID.randomUUID.toString, aReadCnxn, aMessage, bProfileData)

                // send A's IntroductionRequest message
                protocolMgr.publishMessage(aWriteCnxn, aIntroRq)

                // listen for A's IntroductionResponse message
                val aIntroRspLabel = IntroductionResponse.toLabel(sessionId, aIntroRq.correlationId)
                protocolMgr.getMessage(aReadCnxn, aIntroRspLabel, {
                  case IntroductionResponse(_, _, aAccepted, aConnectCorrelationId) =>
                    val bIntroRq = IntroductionRequest(sessionId, UUID.randomUUID.toString, bReadCnxn, bMessage, aProfileData)

                    // send B's IntroductionRequest message
                    protocolMgr.publishMessage(bWriteCnxn, bIntroRq)

                    // listen for B's IntroductionResponse message
                    val bIntroRspLabel = IntroductionResponse.toLabel(sessionId, bIntroRq.correlationId)
                    protocolMgr.getMessage(bReadCnxn, bIntroRspLabel, {
                      case IntroductionResponse(_, _, bAccepted, bConnectCorrelationId) =>
                        // check whether A and B accepted
                        if (aAccepted && bAccepted) {
                          // create new cnxns
                          val cnxnLabel = UUID.randomUUID().toString
                          val abCnxn = PortableAgentCnxn(aReadCnxn.src, cnxnLabel, bReadCnxn.src)
                          val baCnxn = PortableAgentCnxn(bReadCnxn.src, cnxnLabel, aReadCnxn.src)
                          val aNewBiCnxn = PortableAgentBiCnxn(baCnxn, abCnxn)
                          val bNewBiCnxn = PortableAgentBiCnxn(abCnxn, baCnxn)

                          val aConnect = Connect(sessionId, aConnectCorrelationId.get, false, Some(aNewBiCnxn))
                          val bConnect = Connect(sessionId, bConnectCorrelationId.get, false, Some(bNewBiCnxn))

                          // send Connect messages
                          protocolMgr.putMessage(aWriteCnxn, aConnect)
                          protocolMgr.putMessage(bWriteCnxn, bConnect)
                        } else if (aAccepted) {
                          val aConnect = Connect(sessionId, aConnectCorrelationId.get, true, None)

                          // send Connect message
                          protocolMgr.putMessage(aWriteCnxn, aConnect)
                        } else if (bAccepted) {
                          val bConnect = Connect(sessionId, bConnectCorrelationId.get, true, None)

                          // send Connect message
                          protocolMgr.putMessage(bWriteCnxn, bConnect)
                        }
                    })
                })
            })
        })
    })
  }
}

class IntroductionInitiator extends IntroductionInitiatorT {
  override def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    super.run(kvdbNode, cnxns, filters)
  }
}
