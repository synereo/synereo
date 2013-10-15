package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.protegra_ati.agentservices.protocols.msgs._
import java.net.URI
import java.util.{Date, UUID}
import scala.util.continuations._

class IntroductionProtocol extends Serializable {
  def genericIntroducer(
    kvdbNode: Being.AgentKVDBNode[Nothing, Nothing], //PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    aliasCnxn: acT.AgentCnxn) {

    reset {
      // listen for BeginIntroductionRequest message
      for (birq <- kvdbNode.subscribe(aliasCnxn)(new BeginIntroductionRequest().toCnxnCtxtLabel)) {

        birq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, BeginIntroductionRequest(
          Some(sessionId),
          Some(acT.AgentBiCnxn(aReadCnxn, aWriteCnxn)),
          Some(acT.AgentBiCnxn(bReadCnxn, bWriteCnxn)),
          aMessage,
          bMessage)))), _)) => {

            // create A's GetIntroductionProfileRequest message
            val aGetIntroProfileRq = new GetIntroductionProfileRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(aReadCnxn))

            // send A's GetIntroductionProfileRequest message
            reset { kvdbNode.publish(aWriteCnxn)(aGetIntroProfileRq.toCnxnCtxtLabel, aGetIntroProfileRq.toGround) }

            reset {
              // listen for A's GetIntroductionProfileResponse message
              for (agiprsp <- kvdbNode.get(
                aReadCnxn)(
                new GetIntroductionProfileResponse(Some(sessionId), aGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel)) {

                // match response from A
                // TODO: Get introduction profile from message
                agiprsp match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_, _)))), _)) => {

                    // create B's GetIntroductionProfileRequest message
                    val bGetIntroProfileRq = new GetIntroductionProfileRequest(
                      Some(sessionId),
                      Some(UUID.randomUUID.toString),
                      Some(bReadCnxn))

                    // send B's GetIntroductionProfileRequest message
                    reset { kvdbNode.publish(bWriteCnxn)(bGetIntroProfileRq.toCnxnCtxtLabel, bGetIntroProfileRq.toGround) }

                    reset {
                      // listen for B's GetIntroductionProfileResponse message
                      for (bgiprsp <- kvdbNode.get(
                        bReadCnxn)(
                        new GetIntroductionProfileResponse(Some(sessionId), bGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel)) {

                        // match response from B
                        // TODO: Get introduction profile from message
                        bgiprsp match {
                          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_, _)))), _)) => {

                            // create A's IntroductionRequest message
                            // TODO: Add introduction profile to message
                            val aIntroRq = new IntroductionRequest(
                              Some(sessionId),
                              Some(UUID.randomUUID.toString),
                              Some(aReadCnxn), aMessage)

                            // send A's IntroductionRequest message
                            reset { kvdbNode.publish(aWriteCnxn)(aIntroRq.toCnxnCtxtLabel, aIntroRq.toGround) }

                            reset {
                              // listen for A's IntroductionResponse message
                              for (airsp <- kvdbNode.get(
                                aReadCnxn)(
                                new IntroductionResponse(Some(sessionId), aIntroRq.correlationId.get).toCnxnCtxtLabel)) {

                                // match response from A
                                airsp match {
                                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                  _,
                                  _,
                                  Some(aAccepted),
                                  aCnxnName,
                                  aRejectReason,
                                  Some(aConnectId))))), _)) => {

                                    // create B's IntroductionRequest message
                                    // TODO: Add introduction profile to message
                                    val bIntroRq = new IntroductionRequest(
                                      Some(sessionId),
                                      Some(UUID.randomUUID.toString),
                                      Some(bReadCnxn), bMessage)

                                    // send B's IntroductionRequest message
                                    reset { kvdbNode.publish(bWriteCnxn)(bIntroRq.toCnxnCtxtLabel, bIntroRq.toGround) }

                                    reset {
                                      // listen for B's IntroductionResponse message
                                      for (birsp <- kvdbNode.get(
                                        bReadCnxn)(
                                        new IntroductionResponse(Some(sessionId), bIntroRq.correlationId.get).toCnxnCtxtLabel)) {

                                        // match response from B
                                        birsp match {
                                          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                          _,
                                          _,
                                          Some(bAccepted),
                                          bCnxnName,
                                          bRejectReason,
                                          Some(bConnectId))))), _)) => {

                                            // check whether A and B accepted
                                            if (aAccepted && bAccepted) {
                                              // create new cnxns
                                              // TODO: Create new cnxns properly
                                              val aURI = new URI("cnxn://" + UUID.randomUUID().toString)
                                              val bURI = new URI("cnxn://" + UUID.randomUUID().toString)
                                              val abCnxn = new acT.AgentCnxn(aURI, "", bURI)
                                              val baCnxn = new acT.AgentCnxn(bURI, "", aURI)
                                              val aNewBiCnxn = new acT.AgentBiCnxn(baCnxn, abCnxn)
                                              val bNewBiCnxn = new acT.AgentBiCnxn(abCnxn, baCnxn)

                                              // create Connect messages
                                              val aConnect = new Connect(Some(sessionId), aConnectId, aCnxnName, Some(aNewBiCnxn))
                                              val bConnect = new Connect(Some(sessionId), bConnectId, bCnxnName, Some(bNewBiCnxn))

                                              // send Connect messages
                                              reset { kvdbNode.put(aWriteCnxn)(aConnect.toCnxnCtxtLabel, aConnect.toGround) }
                                              reset { kvdbNode.put(bWriteCnxn)(bConnect.toCnxnCtxtLabel, bConnect.toGround) }
                                            }
                                          }
                                          case None => {}
                                          case _ => {
                                            // expected IntroductionResponse
                                            throw new Exception("unexpected protocol message")
                                          }
                                        }
                                      }
                                    }
                                  }
                                  case None => {}
                                  case _ => {
                                    // expected IntroductionResponse
                                    throw new Exception("unexpected protocol message")
                                  }
                                }
                              }
                            }
                          }
                          case None => {}
                          case _ => {
                            // expected GetIntroductionProfileResponse
                            throw new Exception("unexpected protocol message")
                          }
                        }
                      }
                    }
                  }
                  case None => {}
                  case _ => {
                    // expected GetIntroductionProfileResponse
                    throw new Exception("unexpected protocol message")
                  }
                }
              }
            }
          }
          case None => {}
          case e => {
            // expected BeginIntroductionRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }
  }

  def genericIntroduced(
    kvdbNode: Being.AgentKVDBNode[Nothing, Nothing], //PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    readCnxn: acT.AgentCnxn,
    aliasCnxn: acT.AgentCnxn) {

    reset {
      // listen for GetIntroductionProfileRequest message
      for (giprq <- kvdbNode.subscribe(readCnxn)(new GetIntroductionProfileRequest().toCnxnCtxtLabel)) {

        giprq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileRequest(
            Some(sessionId),
            Some(corrId),
            Some(rspCnxn))))), _)) => {

            // TODO: Load introduction profile

            // create GetIntroductionProfileResponse message
            // TODO: Set introduction profile on message
            val getIntroProfileRsp = new GetIntroductionProfileResponse(Some(sessionId), corrId)

            // send GetIntroductionProfileResponse message
            Thread.sleep(1000)
            reset { kvdbNode.put(rspCnxn)(getIntroProfileRsp.toCnxnCtxtLabel, getIntroProfileRsp.toGround) }
          }
          case None => {}
          case _ => {
            // expected GetIntroductionProfileRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }

    reset {
      // listen for IntroductionRequest message
      for (irq <- kvdbNode.subscribe(readCnxn)(new IntroductionRequest().toCnxnCtxtLabel)) {

        irq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionRequest(
            Some(sessionId),
            Some(corrId),
            Some(rspCnxn),
            message)))), _)) => {

            // create IntroductionRequest message
            // TODO: Add introduction profile to message
            val introRq = new IntroductionRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(aliasCnxn), message)

            // send IntroductionRequest message
            reset { kvdbNode.put(aliasCnxn)(introRq.toCnxnCtxtLabel, introRq.toGround) }

            reset {
              // listen for IntroductionResponse message
              for (irsp <- kvdbNode.get(
                aliasCnxn)(
                new IntroductionResponse(Some(sessionId), introRq.correlationId.get).toCnxnCtxtLabel)) {

                irsp match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                  _,
                  _,
                  Some(accepted),
                  cnxnName,
                  rejectReason,
                  _)))), _)) => {

                    // create IntroductionResponse message
                    val introRsp = new IntroductionResponse(
                      Some(sessionId),
                      corrId,
                      Some(accepted),
                      cnxnName,
                      rejectReason,
                      Some(UUID.randomUUID.toString))

                    // send IntroductionResponse message
                    reset { kvdbNode.put(rspCnxn)(introRsp.toCnxnCtxtLabel, introRsp.toGround) }

                    if (accepted) {
                      reset {
                        // listen for Connect message
                        for (connect <- kvdbNode.get(
                          readCnxn)(
                          new Connect(Some(sessionId), introRsp.connectId.get).toCnxnCtxtLabel)) {

                          connect match {
                            case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Connect(
                            _,
                            _,
                            Some(cnxnName),
                            Some(acT.AgentBiCnxn(newReadCnxn, newWriteCnxn)))))), _)) => {

                              // TODO: Register behaviors on new request cnxn
                              reset {
                                // get the list of cnxns
                                for (cnxns <- kvdbNode.get(aliasCnxn)(new Cnxns().toCnxnCtxtLabel)) {
                                  cnxns match {
                                    case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Cnxns(
                                    _,
                                    cnxnList)))), _)) => {

                                      // create new Cnxns object with the new cnxns
                                      val cnxns = new Cnxns(Some(new Date()), (newReadCnxn, newWriteCnxn) :: cnxnList)

                                      // save new Cnxns object
                                      reset { kvdbNode.put(aliasCnxn)(cnxns.toCnxnCtxtLabel, cnxns.toGround) }
                                    }
                                    case _ => {
                                      // expected Cnxns
                                      throw new Exception("unexpected data")
                                    }
                                  }
                                }
                              }
                            }
                            case None => {}
                            case _ => {
                              // expected Connect
                              throw new Exception("unexpected protocol message")
                            }
                          }
                        }
                      }
                    }
                  }
                  case None => {}
                  case _ => {
                    // expected IntroductionResponse
                    throw new Exception("unexpected protocol message")
                  }
                }
              }
            }
          }
          case None => {}
          case _ => {
            // expected IntroductionRequest
            throw new Exception("unexpected protocol message")
          }
        }
      }
    }
  }
}
