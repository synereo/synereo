package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

trait IntroductionRecipientT extends Serializable {
  val biCnxnsListLabel = "biCnxnsList(true)".toLabel
  val profileDataLabel = "jsonBlob(W)".toLabel

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

  private def listenGetIntroductionProfileRequest(
    protocolMgr: ProtocolManager,
    readCnxn: PortableAgentCnxn,
    aliasCnxn: PortableAgentCnxn
  ): Unit = {
    // listen for GetIntroductionProfileRequest message
    protocolMgr.subscribeMessage(readCnxn, new GetIntroductionProfileRequest().toCnxnCtxtLabel, {
      case GetIntroductionProfileRequest(Some(sessionId), Some(corrId), Some(rspCnxn)) =>
        // TODO: get data from aliasCnxn once it is being stored there
        val identityCnxn = PortableAgentCnxn(aliasCnxn.src, "identity", aliasCnxn.trgt)

        // get the profile data
        protocolMgr.read(identityCnxn, profileDataLabel, {
          case Some(mTT.Ground(PostedExpr(jsonBlob: String))) =>
            // create GetIntroductionProfileResponse message
            val getIntroProfileRsp = new GetIntroductionProfileResponse(Some(sessionId), corrId, Some(jsonBlob))

            // send GetIntroductionProfileResponse message
            protocolMgr.putMessage(rspCnxn, getIntroProfileRsp.toCnxnCtxtLabel, getIntroProfileRsp)
          case _ => throw new Exception("unexpected profile data")
        })
    })
  }

  private def listenIntroductionRequest(
    protocolMgr: ProtocolManager,
    readCnxn: PortableAgentCnxn,
    aliasCnxn: PortableAgentCnxn
  ): Unit = {
    // listen for IntroductionRequest message
    protocolMgr.subscribeMessage(readCnxn, new IntroductionRequest().toCnxnCtxtLabel, {
      case IntroductionRequest(Some(sessionId), Some(corrId), Some(rspCnxn), message, Some(profileData)) =>
        // create IntroductionNotification message
        val introN = new IntroductionNotification(
          Some(sessionId),
          UUID.randomUUID.toString,
          PortableAgentBiCnxn(readCnxn, rspCnxn),
          message,
          profileData
        )

        // send IntroductionNotification message
        protocolMgr.publishMessage(aliasCnxn, introN.toCnxnCtxtLabel, introN)

        // listen for IntroductionConfirmation message
        protocolMgr.getMessage(aliasCnxn, new IntroductionConfirmation(Some(sessionId), introN.correlationId).toCnxnCtxtLabel, {
          case IntroductionConfirmation(_, _, Some(accepted)) =>
            // create IntroductionResponse message
            val introRsp = new IntroductionResponse(
              Some(sessionId),
              corrId,
              Some(accepted),
              Some(UUID.randomUUID.toString)
            )

            // send IntroductionResponse message
            protocolMgr.putMessage(rspCnxn, introRsp.toCnxnCtxtLabel, introRsp)

            if (accepted) {
              // listen for Connect message
              protocolMgr.getMessage(readCnxn, new Connect(Some(sessionId), introRsp.connectId.get).toCnxnCtxtLabel, {
                case Connect(_, _, Some(cancelled), Some(newBiCnxn)) =>
                  if (!cancelled) {
                    // get the list of biCnxns
                    protocolMgr.get(aliasCnxn, biCnxnsListLabel, {
                      case Some(mTT.Ground(PostedExpr(prevBiCnxns: String))) =>
                        // add new biCnxn to the list
                        val newBiCnxns = newBiCnxn :: Serializer.deserialize[List[PortableAgentBiCnxn]](prevBiCnxns)

                        // serialize biCnxn list
                        val newBiCnxnsStr = Serializer.serialize(newBiCnxns)

                        // save new list of biCnxns
                        protocolMgr.put(aliasCnxn, biCnxnsListLabel, mTT.Ground(PostedExpr(newBiCnxnsStr)))

                        // create ConnectNotification message
                        val connectN = new ConnectNotification(Some(sessionId), newBiCnxn, profileData)

                        // send ConnectNotification message
                        protocolMgr.publishMessage(aliasCnxn, connectN.toCnxnCtxtLabel, connectN)
                      case _ => throw new Exception("unexpected biCnxnsList data")
                    })
                  }
              })
            }
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
