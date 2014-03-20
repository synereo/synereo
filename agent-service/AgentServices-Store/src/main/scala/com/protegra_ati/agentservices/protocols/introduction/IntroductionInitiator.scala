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
    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\nIntroductionInitiator -- entering run method " 
        + "\nnode: " + node
        + "\ncnxns: " + cnxns
        + "\nfilters " + filters
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )
    if (cnxns.size != 1) throw new Exception("invalid number of cnxns supplied")

    val protocolMgr = new ProtocolManager(node)
    val aliasCnxn = cnxns(0)

    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\nIntroductionInitiator -- invoking listenBeginIntroductionRequest " 
        + "\nnode: " + protocolMgr.node
        + "\ncnxn: " + aliasCnxn
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )

    listenBeginIntroductionRequest(protocolMgr, aliasCnxn)
  }

  private def listenBeginIntroductionRequest(protocolMgr: ProtocolManager, aliasCnxn: PortableAgentCnxn): Unit = {
    // listen for BeginIntroductionRequest message
    val beginIntroRqLabel = BeginIntroductionRequest.toLabel()
    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\nIntroductionInitiator -- waiting for BeginIntroductionRequest " 
        + "\nnode: " + protocolMgr.node
        + "\ncnxn: " + aliasCnxn
        + "\nbeginIntroRqLabel: " + beginIntroRqLabel
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )
    protocolMgr.subscribeMessage(aliasCnxn, beginIntroRqLabel, {
      case bIRRq@BeginIntroductionRequest(sessionId, PortableAgentBiCnxn(aReadCnxn, aWriteCnxn), PortableAgentBiCnxn(bReadCnxn, bWriteCnxn), aMessage, bMessage) => {
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionInitiator -- received BeginIntroductionRequest " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + aliasCnxn
            + "\nbeginIntroRqLabel: " + beginIntroRqLabel
            + "\nBeginIntroductionRequest: " + bIRRq
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        val aGetIntroProfileRq = GetIntroductionProfileRequest(sessionId, UUID.randomUUID.toString, aReadCnxn)

        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionInitiator -- sending A's GetIntroductionProfileRequest " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + aWriteCnxn
            + "\naGetIntroProfileRqLabel: " + aGetIntroProfileRq.toLabel
            + "\nGetIntroductionProfileRequest: " + aGetIntroProfileRq
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        
        // send A's GetIntroductionProfileRequest message
        protocolMgr.publishMessage(aWriteCnxn, aGetIntroProfileRq)
        
        // listen for A's GetIntroductionProfileResponse message
        val aGetIntroProfileRspLabel = GetIntroductionProfileResponse.toLabel(sessionId, aGetIntroProfileRq.correlationId)
        
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\nIntroductionInitiator -- waiting for A's GetIntroductionProfileResponse " 
            + "\nnode: " + protocolMgr.node
            + "\ncnxn: " + aReadCnxn
            + "\naGetIntroProfileRspLabel: " + aGetIntroProfileRspLabel
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        protocolMgr.getMessage(aReadCnxn, aGetIntroProfileRspLabel, {
          case aGIRPRsp@GetIntroductionProfileResponse(_, _, aProfileData) => {
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionInitiator -- received A's GetIntroductionProfileResponse " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + aReadCnxn
                + "\naGetIntroProfileRspLabel: " + aGetIntroProfileRspLabel
                + "\nGetIntroductionProfileResponse: " + aGIRPRsp
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            val bGetIntroProfileRq = GetIntroductionProfileRequest(sessionId, UUID.randomUUID.toString, bReadCnxn)
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionInitiator -- sending B's GetIntroductionProfileRequest " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + bWriteCnxn
                + "\naGetIntroProfileRqLabel: " + bGetIntroProfileRq.toLabel
                + "\nGetIntroductionProfileRequest: " + bGetIntroProfileRq
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            
            
            // send B's GetIntroductionProfileRequest message
            protocolMgr.publishMessage(bWriteCnxn, bGetIntroProfileRq)
            
            // listen for B's GetIntroductionProfileResponse message
            val bGetIntroProfileRspLabel = GetIntroductionProfileResponse.toLabel(sessionId, bGetIntroProfileRq.correlationId)
            
            println(
              (
                "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                + "\nIntroductionInitiator -- waiting for B's GetIntroductionProfileResponse " 
                + "\nnode: " + protocolMgr.node
                + "\ncnxn: " + bReadCnxn
                + "\naGetIntroProfileRspLabel: " + bGetIntroProfileRspLabel
                + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              )
            )
            protocolMgr.getMessage(bReadCnxn, bGetIntroProfileRspLabel, {          
              case bGIRPRsp@GetIntroductionProfileResponse(_, _, bProfileData) => {
                println(
                  (
                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    + "\nIntroductionInitiator -- received B's GetIntroductionProfileResponse " 
                    + "\nnode: " + protocolMgr.node
                    + "\ncnxn: " + bReadCnxn
                    + "\naGetIntroProfileRspLabel: " + bGetIntroProfileRspLabel
                    + "\nGetIntroductionProfileResponse: " + bGIRPRsp
                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  )
                )
                val aIntroRq = IntroductionRequest(sessionId, UUID.randomUUID.toString, aReadCnxn, aMessage, bProfileData)
                
                println(
                  (
                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    + "\nIntroductionInitiator -- sending A's IntroductionRequest " 
                    + "\nnode: " + protocolMgr.node
                    + "\ncnxn: " + aWriteCnxn
                    + "\naIntroRqLabel: " + aIntroRq.toLabel
                    + "\nIntroductionRequest: " + aIntroRq
                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  )
                )
                // send A's IntroductionRequest message
                protocolMgr.publishMessage(aWriteCnxn, aIntroRq)
                
                // listen for A's IntroductionResponse message
                val aIntroRspLabel = IntroductionResponse.toLabel(sessionId, aIntroRq.correlationId)
                println(
                  (
                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    + "\nIntroductionInitiator -- waiting for A's IntroductionResponse " 
                    + "\nnode: " + protocolMgr.node
                    + "\ncnxn: " + aReadCnxn
                    + "\naIntroRspLabel: " + aIntroRspLabel
                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  )
                )
                
                protocolMgr.getMessage(aReadCnxn, aIntroRspLabel, {
                  case aIR@IntroductionResponse(_, _, aAccepted, aConnectCorrelationId) => {
                    println(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nIntroductionInitiator -- received A's IntroductionResponse " 
                        + "\nnode: " + protocolMgr.node
                        + "\ncnxn: " + aReadCnxn
                        + "\naGetIntroProfileRspLabel: " + aIntroRspLabel
                        + "\nGetIntroductionProfileResponse: " + aIR
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
                    val bIntroRq = IntroductionRequest(sessionId, UUID.randomUUID.toString, bReadCnxn, bMessage, aProfileData)
                    
                    println(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nIntroductionInitiator -- sending b's IntroductionRequest " 
                        + "\nnode: " + protocolMgr.node
                        + "\ncnxn: " + bWriteCnxn
                        + "\nbIntroRqLabel: " + bIntroRq.toLabel
                        + "\nIntroductionRequest: " + bIntroRq
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
                    
                    // send B's IntroductionRequest message
                    protocolMgr.publishMessage(bWriteCnxn, bIntroRq)
                    
                    // listen for B's IntroductionResponse message
                    val bIntroRspLabel = IntroductionResponse.toLabel(sessionId, bIntroRq.correlationId)
                    println(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nIntroductionInitiator -- waiting for B's IntroductionResponse " 
                        + "\nnode: " + protocolMgr.node
                        + "\ncnxn: " + bReadCnxn
                        + "\naIntroRspLabel: " + bIntroRspLabel
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
                    
                    protocolMgr.getMessage(bReadCnxn, bIntroRspLabel, {
                      case bIR@IntroductionResponse(_, _, bAccepted, bConnectCorrelationId) => {
                        println(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\nIntroductionInitiator -- received B's IntroductionResponse " 
                            + "\nnode: " + protocolMgr.node
                            + "\ncnxn: " + bReadCnxn
                            + "\naGetIntroProfileRspLabel: " + bIntroRspLabel
                            + "\nGetIntroductionProfileResponse: " + bIR
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        
                        
                        println(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\nIntroductionInitiator -- checking transaction status " 
                            + "\nnode: " + protocolMgr.node
                            + "\naAccepted: " + aAccepted
                            + "\nbAccepted: " + bAccepted
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        // check whether A and B accepted
                        if (aAccepted && bAccepted) {
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- both parties committed " 
                              + "\nnode: " + protocolMgr.node
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          // create new cnxns
                          val cnxnLabel = UUID.randomUUID().toString
                          val abCnxn = PortableAgentCnxn(aReadCnxn.src, cnxnLabel, bReadCnxn.src)
                          val baCnxn = PortableAgentCnxn(bReadCnxn.src, cnxnLabel, aReadCnxn.src)
                          val aNewBiCnxn = PortableAgentBiCnxn(baCnxn, abCnxn)
                          val bNewBiCnxn = PortableAgentBiCnxn(abCnxn, baCnxn)
                          
                          val aConnect = Connect(sessionId, aConnectCorrelationId.get, false, Some(aNewBiCnxn))
                          val bConnect = Connect(sessionId, bConnectCorrelationId.get, false, Some(bNewBiCnxn))
                          
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- sending both Connect Msgs" 
                              + "\nnode: " + protocolMgr.node
                              + "\naCnxn: " + aWriteCnxn
                              + "\nbCnxn: " + bWriteCnxn
                              + "\naConnectLabel: " + aConnect.toLabel
                              + "\nbConnectLabel: " + bConnect.toLabel
                              + "\naConnect: " + aConnect
                              + "\nbConnect: " + bConnect
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          
                          // send Connect messages
                          protocolMgr.putMessage(aWriteCnxn, aConnect)
                          protocolMgr.putMessage(bWriteCnxn, bConnect)
                        } else if (aAccepted) {
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- A committed; B didn't " 
                              + "\nnode: " + protocolMgr.node
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          val aConnect = Connect(sessionId, aConnectCorrelationId.get, true, None)
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- alerting A's UI" 
                              + "\nnode: " + protocolMgr.node
                              + "\naCnxn: " + aWriteCnxn
                              + "\naConnectLabel: " + aConnect.toLabel
                              + "\naConnect: " + aConnect
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          
                          // send Connect message
                          protocolMgr.putMessage(aWriteCnxn, aConnect)
                        } else if (bAccepted) {
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- B committed; A didn't " 
                              + "\nnode: " + protocolMgr.node
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          val bConnect = Connect(sessionId, bConnectCorrelationId.get, true, None)
                          println(
                            (
                              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                              + "\nIntroductionInitiator -- alerting B's UI" 
                              + "\nnode: " + protocolMgr.node
                              + "\naCnxn: " + bWriteCnxn
                              + "\naConnectLabel: " + bConnect.toLabel
                              + "\naConnect: " + bConnect
                              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            )
                          )
                          
                          
                          // send Connect message
                          protocolMgr.putMessage(bWriteCnxn, bConnect)
                        }
                      }
                    })
                  }
                })
              }
            })
          }
        })
      }
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
