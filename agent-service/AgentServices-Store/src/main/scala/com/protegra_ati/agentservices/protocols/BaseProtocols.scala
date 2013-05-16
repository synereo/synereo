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
            Some(biRequestId),
            Some(biResponseCnxn),
            Some(aRequestCnxn),
            Some(aResponseCnxn),
            Some(bRequestCnxn),
            Some(bResponseCnxn),
            aMessage,
            bMessage)))) => {

            // create GetIntroductionProfileRequest messages
            val aGetIntroductionProfileRequest = new GetIntroductionProfileRequest(Some(UUID.randomUUID.toString), Some(aResponseCnxn))
            val bGetIntroductionProfileRequest = new GetIntroductionProfileRequest(Some(UUID.randomUUID.toString), Some(bResponseCnxn))

            // send GetIntroductionProfileRequest messages
            kvdbNode.put(aRequestCnxn)(aGetIntroductionProfileRequest.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(aGetIntroductionProfileRequest.toCnxnCtxtLabel, Nil, aGetIntroductionProfileRequest)))
            kvdbNode.put(bRequestCnxn)(bGetIntroductionProfileRequest.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(bGetIntroductionProfileRequest.toCnxnCtxtLabel, Nil, bGetIntroductionProfileRequest)))

            reset {
              // listen for GetIntroductionProfileResponse messages
              // TODO: TTL
              for (
                agiprsp <- kvdbNode.get(aResponseCnxn)(new GetIntroductionProfileResponse(aGetIntroductionProfileRequest.requestId.get).toCnxnCtxtLabel);
                bgiprsp <- kvdbNode.get(bResponseCnxn)(new GetIntroductionProfileResponse(bGetIntroductionProfileRequest.requestId.get).toCnxnCtxtLabel)) {

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
                        val aIntroductionRequest = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(aResponseCnxn), aMessage)
                        val bIntroductionRequest = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(bResponseCnxn), bMessage)

                        // send IntroductionRequest messages
                        kvdbNode.put(aRequestCnxn)(aIntroductionRequest.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(aIntroductionRequest.toCnxnCtxtLabel, Nil, aIntroductionRequest)))
                        kvdbNode.put(bRequestCnxn)(bIntroductionRequest.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(bIntroductionRequest.toCnxnCtxtLabel, Nil, bIntroductionRequest)))

                        reset {
                          // listen for IntroductionResponse messages
                          // TODO: TTL
                          // TODO: Find a way to stop if either party rejects
                          for (
                            airsp <- kvdbNode.get(aResponseCnxn)(new IntroductionResponse(aIntroductionRequest.requestId.get).toCnxnCtxtLabel);
                            birsp <- kvdbNode.get(bResponseCnxn)(new IntroductionResponse(bIntroductionRequest.requestId.get).toCnxnCtxtLabel)) {

                            // match response from A
                            airsp match {
                              case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(_, Some(aAccepted), aRejectReason, Some(aConnectId))))) => {

                                // match response from B
                                birsp match {
                                  case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(_, Some(bAccepted), bRejectReason, Some(bConnectId))))) => {

                                    // check whether A and B accepted
                                    // TODO: Uncomment if below and fix compiler error
                                    //if (aAccepted && bAccepted) {
                                      // create new cnxns
                                      // TODO: Create new cnxns properly
                                      val abCnxn = new acT.AgentCnxn(new URI("agent://a"), "", new URI("agent://b"))
                                      val baCnxn = new acT.AgentCnxn(new URI("agent://b"), "", new URI("agent://a"))

                                      // create Connect messages
                                      val aConnect = new Connect(aConnectId, Some(abCnxn), Some(baCnxn))
                                      val bConnect = new Connect(bConnectId, Some(baCnxn), Some(abCnxn))

                                      // send Connect messages
                                      kvdbNode.put(aRequestCnxn)(aConnect.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(aConnect.toCnxnCtxtLabel, Nil, aConnect)))
                                      kvdbNode.put(bRequestCnxn)(bConnect.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(bConnect.toCnxnCtxtLabel, Nil, bConnect)))
                                    //}

                                    // create BeginIntroductionResponse message
                                    // TODO: Set aRejectReason and bRejectReason
                                    val beginIntroductionResponse = new BeginIntroductionResponse(biRequestId, Some(aAccepted && bAccepted))

                                    // send BeginIntroductionResponse message
                                    kvdbNode.put(biResponseCnxn)(beginIntroductionResponse.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(beginIntroductionResponse.toCnxnCtxtLabel, Nil, beginIntroductionResponse)))
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
    cnxn: acT.AgentCnxn) {

    reset {
      // listen for GetIntroductionProfileRequest message
      for (giprq <- kvdbNode.get(cnxn)(new GetIntroductionProfileRequest().toCnxnCtxtLabel)) {

        giprq match {
          case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileRequest(Some(requestId), Some(responseCnxn))))) => {
            // TODO: Load introduction profile

            // create GetIntroductionProfileResponse message
            // TODO: Set introduction profile on message
            val getIntroductionProfileResponse = new GetIntroductionProfileResponse(requestId)

            // send GetIntroductionProfileResponse message
            kvdbNode.put(responseCnxn)(getIntroductionProfileResponse.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(getIntroductionProfileResponse.toCnxnCtxtLabel, Nil, getIntroductionProfileResponse)))
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
          case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionRequest(Some(requestId), Some(responseCnxn), message)))) => {

            // create IntroductionRequest message
            // TODO: Add introduction profile to message
            // TODO: Figure out where the response cnxn comes from
            val introductionRequest = new IntroductionRequest(Some(UUID.randomUUID.toString), Some(responseCnxn), message)

            // send IntroductionRequest message
            // TODO: Figure out where the request cnxn comes from
            kvdbNode.put(cnxn)(introductionRequest.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(introductionRequest.toCnxnCtxtLabel, Nil, introductionRequest)))

            reset {
              // listen for IntroductionResponse message
              // TODO: TTL
              // TODO: Figure out where the response cnxn comes from
              for (irsp <- kvdbNode.get(responseCnxn)(new IntroductionResponse(introductionRequest.requestId.get).toCnxnCtxtLabel)) {

                irsp match {
                  case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(_, Some(accepted), rejectReason, _)))) => {

                    // create IntroductionResponse message
                    val introductionResponse = new IntroductionResponse(requestId, Some(accepted), rejectReason, Some(UUID.randomUUID.toString))

                    // send IntroductionResponse message
                    kvdbNode.put(responseCnxn)(introductionResponse.toCnxnCtxtLabel, mTT.Ground(ConcreteHL.InsertContent(introductionResponse.toCnxnCtxtLabel, Nil, introductionResponse)))

                    if (accepted) {
                      reset {
                        // listen for Connect message
                        // TODO: TTL
                        for (connect <- kvdbNode.get(cnxn)(new Connect(introductionResponse.connectId.get).toCnxnCtxtLabel)) {

                          connect match {
                            case Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Connect(_, Some(newRequestCnxn), Some(newResponseCnxn))))) => {
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
