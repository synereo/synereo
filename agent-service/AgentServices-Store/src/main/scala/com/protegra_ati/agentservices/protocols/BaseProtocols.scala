package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import java.net.URI
import java.util.UUID
import scala.util.continuations._

trait BaseProtocols {
  def genericIntroducer(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxn: acT.AgentCnxn) {

    reset {
      // listen for BeginIntroductionRequest message
      for (birq <- kvdbNode.get(cnxn)(new BeginIntroductionRequest().toCnxnCtxtLabel)) {

        birq match {
          case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, BeginIntroductionRequest(
            Some(biRqId),
            Some(biRspCnxn),
            Some(aRqCnxn),
            Some(aRspCnxn),
            Some(bRqCnxn),
            Some(bRspCnxn),
            aMessage,
            bMessage)))) => {

            // create GetIntroductionProfileRequest messages
            val aGetIntroProfileRq = new GetIntroductionProfileRequest(Some(UUID.randomUUID.toString), Some(aRspCnxn))
            val bGetIntroProfileRq = new GetIntroductionProfileRequest(Some(UUID.randomUUID.toString), Some(bRspCnxn))

            // send GetIntroductionProfileRequest messages
            reset { kvdbNode.put(aRqCnxn)(aGetIntroProfileRq.toCnxnCtxtLabel, aGetIntroProfileRq.toGround) }
            reset { kvdbNode.put(bRqCnxn)(bGetIntroProfileRq.toCnxnCtxtLabel, bGetIntroProfileRq.toGround) }

            reset {
              // listen for GetIntroductionProfileResponse messages
              // TODO: TTL
              for (
                agiprsp <- kvdbNode.get
                  (aRspCnxn)
                  (new GetIntroductionProfileResponse(aGetIntroProfileRq.requestId.get).toCnxnCtxtLabel);
                bgiprsp <- kvdbNode.get
                  (bRspCnxn)
                  (new GetIntroductionProfileResponse(bGetIntroProfileRq.requestId.get).toCnxnCtxtLabel)) {

                // match response from A
                // TODO: Get introduction profile from message
                agiprsp match {
                  case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_)))) => {

                    // match response from B
                    // TODO: Get introduction profile from message
                    bgiprsp match {
                      case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_)))) => {

                        // create IntroductionRequest messages
                        // TODO: Add introduction profiles to messages
                        val aIntroRq = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(aRspCnxn), aMessage)
                        val bIntroRq = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(bRspCnxn), bMessage)

                        // send IntroductionRequest messages
                        reset { kvdbNode.put(aRqCnxn)(aIntroRq.toCnxnCtxtLabel, aIntroRq.toGround) }
                        reset { kvdbNode.put(bRqCnxn)(bIntroRq.toCnxnCtxtLabel, bIntroRq.toGround) }

                        reset {
                          // listen for IntroductionResponse messages
                          // TODO: TTL
                          // TODO: Find a way to stop if either party rejects
                          for (
                            airsp <- kvdbNode.get
                              (aRspCnxn)
                              (new IntroductionResponse(aIntroRq.requestId.get).toCnxnCtxtLabel);
                            birsp <- kvdbNode.get
                              (bRspCnxn)
                              (new IntroductionResponse(bIntroRq.requestId.get).toCnxnCtxtLabel)) {

                            // match response from A
                            airsp match {
                              case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                _,
                                Some(aAccepted),
                                aRejectReason,
                                Some(aConnectId))))) => {

                                // match response from B
                                birsp match {
                                  case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                    _,
                                    Some(bAccepted),
                                    bRejectReason,
                                    Some(bConnectId))))) => {

                                    // create BeginIntroductionResponse message
                                    val beginIntroRsp = new BeginIntroductionResponse(
                                      biRqId,
                                      Some(aAccepted && bAccepted),
                                      aRejectReason,
                                      bRejectReason)

                                    // check whether A and B accepted
                                    if (aAccepted && bAccepted) {
                                      // create new cnxns
                                      // TODO: Create new cnxns properly
                                      val abCnxn = new acT.AgentCnxn(new URI("agent://a"), "", new URI("agent://b"))
                                      val baCnxn = new acT.AgentCnxn(new URI("agent://b"), "", new URI("agent://a"))

                                      // create Connect messages
                                      val aConnect = new Connect(aConnectId, Some(abCnxn), Some(baCnxn))
                                      val bConnect = new Connect(bConnectId, Some(baCnxn), Some(abCnxn))

                                      // send Connect messages
                                      reset { kvdbNode.put(aRqCnxn)(aConnect.toCnxnCtxtLabel, aConnect.toGround) }
                                      reset { kvdbNode.put(bRqCnxn)(bConnect.toCnxnCtxtLabel, bConnect.toGround) }

                                      // send BeginIntroductionResponse message
                                      reset { kvdbNode.put(biRspCnxn)(beginIntroRsp.toCnxnCtxtLabel, beginIntroRsp.toGround) }
                                    } else {
                                      // send BeginIntroductionResponse message
                                      reset { kvdbNode.put(biRspCnxn)(beginIntroRsp.toCnxnCtxtLabel, beginIntroRsp.toGround) }
                                    }
                                  }
                                  case _ => {
                                    // expected IntroductionResponse
                                    throw new Exception("unexpected protocol message")
                                  }
                                }
                              }
                              case _ => {
                                // expected IntroductionResponse
                                throw new Exception("unexpected protocol message")
                              }
                            }
                          }
                        }
                      }
                      case _ => {
                        // expected GetIntroductionProfileResponse
                        throw new Exception("unexpected protocol message")
                      }
                    }
                  }
                  case _ => {
                    // expected GetIntroductionProfileResponse
                    throw new Exception("unexpected protocol message")
                  }
                }
              }
            }
          }
          case _ => {
            // expected BeginIntroductionRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }
  }

  def genericIntroduced(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxn: acT.AgentCnxn,
    privateRqCnxn: acT.AgentCnxn,
    privateRspCnxn: acT.AgentCnxn) {

    reset {
      // listen for GetIntroductionProfileRequest message
      for (giprq <- kvdbNode.get(cnxn)(new GetIntroductionProfileRequest().toCnxnCtxtLabel)) {

        giprq match {
          case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileRequest(
            Some(rqId),
            Some(rspCnxn))))) => {

            // TODO: Load introduction profile

            // create GetIntroductionProfileResponse message
            // TODO: Set introduction profile on message
            val getIntroProfileRsp = new GetIntroductionProfileResponse(rqId)

            // send GetIntroductionProfileResponse message
            reset { kvdbNode.put(rspCnxn)(getIntroProfileRsp.toCnxnCtxtLabel, getIntroProfileRsp.toGround) }
          }
          case _ => {
            // expected GetIntroductionProfileRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }

    reset {
      // listen for IntroductionRequest message
      for (irq <- kvdbNode.get(cnxn)(new IntroductionRequest().toCnxnCtxtLabel)) {

        irq match {
          case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionRequest(
            Some(rqId),
            Some(rspCnxn),
            message)))) => {

            // create IntroductionRequest message
            // TODO: Add introduction profile to message
            val introRq = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(privateRspCnxn), message)

            // send IntroductionRequest message
            reset { kvdbNode.put(privateRqCnxn)(introRq.toCnxnCtxtLabel, introRq.toGround) }

            reset {
              // listen for IntroductionResponse message
              // TODO: TTL
              for (irsp <- kvdbNode.get
                (privateRspCnxn)
                (new IntroductionResponse(introRq.requestId.get).toCnxnCtxtLabel)) {

                irsp match {
                  case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                    _,
                    Some(accepted),
                    rejectReason,
                    _)))) => {

                    // create IntroductionResponse message
                    val introRsp = new IntroductionResponse(
                      rqId,
                      Some(accepted),
                      rejectReason,
                      Some(UUID.randomUUID.toString))

                    // send IntroductionResponse message
                    reset { kvdbNode.put(rspCnxn)(introRsp.toCnxnCtxtLabel, introRsp.toGround) }

                    if (accepted) {
                      reset {
                        // listen for Connect message
                        // TODO: TTL
                        for (connect <- kvdbNode.get(cnxn)(new Connect(introRsp.connectId.get).toCnxnCtxtLabel)) {

                          connect match {
                            case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Connect(
                              _,
                              Some(newRqCnxn),
                              Some(newRspCnxn))))) => {

                              // TODO: Store the new cnxns
                            }
                            case _ => {
                              // expected Connect
                              throw new Exception("unexpected protocol message")
                            }
                          }
                        }
                      }
                    }
                  }
                  case _ => {
                    // expected IntroductionResponse
                    throw new Exception("unexpected protocol message")
                  }
                }
              }
            }
          }
          case _ => {
            // expected IntroductionRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }
  }
}
