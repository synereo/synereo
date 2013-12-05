package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution._
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

    // listen for BeginIntroductionRequest message
    protocolMgr.subscribeMessage(aliasCnxn, new BeginIntroductionRequest().toCnxnCtxtLabel, {
      case BeginIntroductionRequest(
        Some(sessionId),
        Some(PortableAgentBiCnxn(aReadCnxn, aWriteCnxn)),
        Some(PortableAgentBiCnxn(bReadCnxn, bWriteCnxn)),
        aMessage,
        bMessage
      ) =>
        // create A's GetIntroductionProfileRequest message
        val aGetIntroProfileRq = new GetIntroductionProfileRequest(
          Some(sessionId),
          Some(UUID.randomUUID.toString),
          Some(aReadCnxn)
        )

        // send A's GetIntroductionProfileRequest message
        protocolMgr.publishMessage(aWriteCnxn, aGetIntroProfileRq.toCnxnCtxtLabel, aGetIntroProfileRq)

        // listen for A's GetIntroductionProfileResponse message
        protocolMgr.getMessage(aReadCnxn, new GetIntroductionProfileResponse(Some(sessionId), aGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel, {
          case GetIntroductionProfileResponse(_, _, Some(aProfileData)) =>
            // create B's GetIntroductionProfileRequest message
            val bGetIntroProfileRq = new GetIntroductionProfileRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(bReadCnxn)
            )

            // send B's GetIntroductionProfileRequest message
            protocolMgr.publishMessage(bWriteCnxn, bGetIntroProfileRq.toCnxnCtxtLabel, bGetIntroProfileRq)

            // listen for B's GetIntroductionProfileResponse message
            protocolMgr.getMessage(bReadCnxn, new GetIntroductionProfileResponse(Some(sessionId), bGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel, {
              case GetIntroductionProfileResponse(_, _, Some(bProfileData)) =>
                // create A's IntroductionRequest message
                val aIntroRq = new IntroductionRequest(
                  Some(sessionId),
                  Some(UUID.randomUUID.toString),
                  Some(aReadCnxn),
                  aMessage,
                  Some(bProfileData)
                )

                // send A's IntroductionRequest message
                protocolMgr.publishMessage(aWriteCnxn, aIntroRq.toCnxnCtxtLabel, aIntroRq)

                // listen for A's IntroductionResponse message
                protocolMgr.getMessage(aReadCnxn, new IntroductionResponse(Some(sessionId), aIntroRq.correlationId.get).toCnxnCtxtLabel, {
                  case IntroductionResponse(_, _, Some(aAccepted), Some(aConnectId)) =>
                    // create B's IntroductionRequest message
                    val bIntroRq = new IntroductionRequest(
                      Some(sessionId),
                      Some(UUID.randomUUID.toString),
                      Some(bReadCnxn),
                      bMessage,
                      Some(aProfileData)
                    )

                    // send B's IntroductionRequest message
                    protocolMgr.publishMessage(bWriteCnxn, bIntroRq.toCnxnCtxtLabel, bIntroRq)

                    // listen for B's IntroductionResponse message
                    protocolMgr.getMessage(bReadCnxn, new IntroductionResponse(Some(sessionId), bIntroRq.correlationId.get).toCnxnCtxtLabel, {
                      case IntroductionResponse(_, _, Some(bAccepted), Some(bConnectId)) =>
                        // check whether A and B accepted
                        if (aAccepted && bAccepted) {
                          // create new cnxns
                          val cnxnLabel = UUID.randomUUID().toString
                          val abCnxn = new PortableAgentCnxn(aReadCnxn.src, cnxnLabel, bReadCnxn.src)
                          val baCnxn = new PortableAgentCnxn(bReadCnxn.src, cnxnLabel, aReadCnxn.src)
                          val aNewBiCnxn = new PortableAgentBiCnxn(baCnxn, abCnxn)
                          val bNewBiCnxn = new PortableAgentBiCnxn(abCnxn, baCnxn)

                          // create Connect messages
                          val aConnect = new Connect(Some(sessionId), aConnectId, Some(false), Some(aNewBiCnxn))
                          val bConnect = new Connect(Some(sessionId), bConnectId, Some(false), Some(bNewBiCnxn))

                          // send Connect messages
                          protocolMgr.putMessage(aWriteCnxn, aConnect.toCnxnCtxtLabel, aConnect)
                          protocolMgr.putMessage(bWriteCnxn, bConnect.toCnxnCtxtLabel, bConnect)
                        } else if (aAccepted) {
                          // create Connect message
                          val aConnect = new Connect(Some(sessionId), aConnectId, Some(true), None)

                          // send Connect message
                          protocolMgr.putMessage(aWriteCnxn, aConnect.toCnxnCtxtLabel, aConnect)
                        } else if (bAccepted) {
                          // create Connect message
                          val bConnect = new Connect(Some(sessionId), bConnectId, Some(true), None)

                          // send Connect message
                          protocolMgr.putMessage(bWriteCnxn, bConnect.toCnxnCtxtLabel, bConnect)
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
