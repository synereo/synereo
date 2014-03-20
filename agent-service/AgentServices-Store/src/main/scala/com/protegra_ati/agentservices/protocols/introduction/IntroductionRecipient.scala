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
  private val profileDataLabel = "jsonBlob(true)".toLabel

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
    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\nIntroductionRecipient -- waiting for GetIntroductionProfileRequest " 
        + "\nnode: " + protocolMgr.node
        + "\ncnxn: " + readCnxn
        + "\ngetIntroProfileRqLabel: " + getIntroProfileRqLabel
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )

    protocolMgr.subscribeMessage(readCnxn, getIntroProfileRqLabel, {
      case gIPR@GetIntroductionProfileRequest(sessionId, correlationId, rspCnxn) => {
        // TODO: get data from aliasCnxn once it is being stored there
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionRecipient -- received GetIntroductionProfileRequest " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + readCnxn
            + "\ngetIntroProfileRqLabel: " + getIntroProfileRqLabel
            + "\nGetIntroductionProfileRequest: " + gIPR
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        val identityCnxn = PortableAgentCnxn(aliasCnxn.src, "identity", aliasCnxn.trgt)

        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionRecipient -- reading profileData " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + identityCnxn
            + "\nprofileDataLabel: " + profileDataLabel
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )

        // get the profile data
        protocolMgr.read(identityCnxn, profileDataLabel, {
          case Some(mTT.RBoundAList(Some(mTT.Ground(PostedExpr(jsonBlob: String))), _)) => {
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- read profileData " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + identityCnxn
                + "\nprofileDataLabel: " + profileDataLabel
                + "\nprofileData: " + jsonBlob
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            val getIntroProfileRsp = GetIntroductionProfileResponse(sessionId, correlationId, jsonBlob)
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- sending GetIntroductionProfileResponse " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + rspCnxn
                + "\nGetIntroProfileRspLabel: " + getIntroProfileRsp.toLabel
                + "\nGetIntroductionProfileResponse: " + getIntroProfileRsp
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            
            // send GetIntroductionProfileResponse message
            protocolMgr.putMessage(rspCnxn, getIntroProfileRsp)
          }
          case _ => throw new Exception("unexpected profile data")
        })
      }
    })
  }

  private def listenIntroductionRequest(protocolMgr: ProtocolManager, readCnxn: PortableAgentCnxn, aliasCnxn: PortableAgentCnxn): Unit = {
    // listen for IntroductionRequest message
    val introRqLabel = IntroductionRequest.toLabel()
    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\nIntroductionRecipient -- waiting for IntroductionRequest " 
        + "\nnode: " + protocolMgr.node
        + "\ncnxn: " + readCnxn
        + "\ngetIntroProfileRqLabel: " + introRqLabel
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )

    protocolMgr.subscribeMessage(readCnxn, introRqLabel, {
      case ir@IntroductionRequest(sessionId, correlationId, rspCnxn, message, profileData) => {
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionRecipient -- received IntroductionRequest " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + readCnxn
            + "\nintroRqLabel: " + introRqLabel
            + "\nIntroductionRequest: " + ir
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )

        val introN = IntroductionNotification(sessionId, UUID.randomUUID.toString, PortableAgentBiCnxn(readCnxn, rspCnxn), message, profileData)
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionRecipient -- sending IntroductionNotification " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + aliasCnxn
            + "\nintroNLabel: " + introN.toLabel
            + "\nIntroductionNotification: " + introN
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        
        // send IntroductionNotification message
        protocolMgr.publishMessage(aliasCnxn, introN)
        
        // listen for IntroductionConfirmation message
        val introConfirmationLabel = IntroductionConfirmation.toLabel(sessionId, introN.correlationId)
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionRecipient -- waiting for IntroductionConfirmation " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + aliasCnxn
            + "\nintroConfirmationLabel: " + introConfirmationLabel
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        
        protocolMgr.getMessage(aliasCnxn, introConfirmationLabel, {
          case ic@IntroductionConfirmation(_, _, true) => {
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- received IntroductionConfirmation " 
                + "\nIntroduction confirmed " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + aliasCnxn
                + "\nintroRqLabel: " + introConfirmationLabel
                + "\nIntroductionRequest: " + ic
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )

            val introRsp = IntroductionResponse(sessionId, correlationId, true, Some(UUID.randomUUID.toString))
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- sending IntroductionResponse " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + rspCnxn
                + "\nintroRspLabel: " + introRsp.toLabel
                + "\nIntroductionResponse: " + introRsp
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )            
            
            // send IntroductionResponse message
            protocolMgr.putMessage(rspCnxn, introRsp)
            
            // listen for Connect message
            val connectLabel = Connect.toLabel(sessionId, introRsp.connectCorrelationId.get)
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- waiting for Connect " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + readCnxn
                + "\nconnectLabel: " + connectLabel
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            
            protocolMgr.getMessage(readCnxn, connectLabel, {
              case c@Connect(_, _, false, Some(newBiCnxn)) => {
                println(
                  (
                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    + "\nIntroductionRecipient -- received Connect " 
                    + "\nnode: " + protocolMgr.node
                    + "\ncnxn: " + readCnxn
                    + "\nconnectLabel: " + connectLabel
                    + "\nConnect: " + c
                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  )
                )

                println(
                  (
                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    + "\nIntroductionRecipient -- retrieving list of biCnxns " 
                    + "\nnode: " + protocolMgr.node
                    + "\ncnxn: " + aliasCnxn
                    + "\nbiCnxnsListLabel: " + biCnxnsListLabel
                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  )
                )

                def handleBiCnxnsList( prevBiCnxns : String ) : Unit = {
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\nIntroductionRecipient -- received biCnxns list " 
                      + "\nnode: " + protocolMgr.node
                      + "\ncnxn: " + aliasCnxn
                      + "\nbiCnxnsListLabel: " + biCnxnsListLabel
                      + "\nprevBiCnxns: " + prevBiCnxns
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  
                  // add new biCnxn to the list
                  val newBiCnxns = newBiCnxn :: Serializer.deserialize[List[PortableAgentBiCnxn]](prevBiCnxns)
                  
                  // serialize biCnxn list
                  val newBiCnxnsStr = Serializer.serialize(newBiCnxns)
                  
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\nIntroductionRecipient -- updating biCnxns list " 
                      + "\nnode: " + protocolMgr.node
                      + "\ncnxn: " + aliasCnxn
                      + "\nbiCnxnsListLabel: " + biCnxnsListLabel
                      + "\nnewBiCnxns: " + newBiCnxns
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  
                  // save new list of biCnxns
                  protocolMgr.put(aliasCnxn, biCnxnsListLabel, mTT.Ground(PostedExpr(newBiCnxnsStr)))
                  
                  val connectN = ConnectNotification(sessionId, newBiCnxn, profileData)
                  
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\nIntroductionRecipient -- sending ConnectNotification " 
                      + "\nnode: " + protocolMgr.node
                      + "\ncnxn: " + aliasCnxn
                      + "\nconnectNLabel: " + connectN.toLabel
                      + "\nConnectionNotification: " + connectN
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  // send ConnectNotification message
                  protocolMgr.publishMessage(aliasCnxn, connectN)
                }
                // get the list of biCnxns
                protocolMgr.get(aliasCnxn, biCnxnsListLabel, {
                  case Some(mTT.Ground(PostedExpr(prevBiCnxns: String))) => {
                    handleBiCnxnsList( prevBiCnxns )
                  }
                  case Some(mTT.RBoundAList(Some(mTT.Ground(PostedExpr(prevBiCnxns: String))), _)) => {
                    handleBiCnxnsList( prevBiCnxns )
                  }
                  case _ => throw new Exception("unexpected biCnxnsList data")
                })
              }
              case Connect(_, _, true, None) => ()
            })
          }
          case icAlt@IntroductionConfirmation(_, _, false) => {
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- received IntroductionConfirmation " 
                + "\nIntroduction denied " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + aliasCnxn
                + "\nintroRqLabel: " + introConfirmationLabel
                + "\nIntroductionRequest: " + icAlt
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            val introRsp = IntroductionResponse(sessionId, correlationId, false, None)
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionRecipient -- sending IntroductionResponse " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + rspCnxn
                + "\nintroRspLabel: " + introRsp.toLabel
                + "\nIntroductionResponse: " + introRsp
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            
            // send IntroductionResponse message
            protocolMgr.putMessage(rspCnxn, introRsp)
          }
        })
      }
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
