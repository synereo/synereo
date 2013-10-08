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
    cnxn: acT.AgentCnxn) {

    reset {
      // listen for BeginIntroductionRequest message
println("listen for BeginIntroductionRequest on " + cnxn)
      for (birq <- kvdbNode.subscribe(cnxn)(new BeginIntroductionRequest().toCnxnCtxtLabel)) {

        birq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, BeginIntroductionRequest(
          Some(sessionId),
          Some(biRqId),
          Some(biRspCnxn),
          Some(aRqCnxn),
          Some(aRspCnxn),
          Some(bRqCnxn),
          Some(bRspCnxn),
          aMessage,
          bMessage)))), _)) => {
println("found BeginIntroductionRequest on " + cnxn)
            // create A's GetIntroductionProfileRequest message
            val aGetIntroProfileRq = new GetIntroductionProfileRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(aRspCnxn))

            // send A's GetIntroductionProfileRequest message
            reset { kvdbNode.publish(aRqCnxn)(aGetIntroProfileRq.toCnxnCtxtLabel, aGetIntroProfileRq.toGround) }
println("sent GetIntroductionProfileRequest on " + aRqCnxn)
            reset {
              // listen for A's GetIntroductionProfileResponse message
println("listen for GetIntroductionProfileResponse on " + aRspCnxn)
              for (agiprsp <- kvdbNode.get(
                aRspCnxn)(
                new GetIntroductionProfileResponse(Some(sessionId), aGetIntroProfileRq.requestId.get).toCnxnCtxtLabel)) {

                // match response from A
                // TODO: Get introduction profile from message
                agiprsp match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_, _)))), _)) => {
println("found GetIntroductionProfileResponse on " + aRspCnxn)
                    // create B's GetIntroductionProfileRequest message
                    val bGetIntroProfileRq = new GetIntroductionProfileRequest(
                      Some(sessionId),
                      Some(UUID.randomUUID.toString),
                      Some(bRspCnxn))

                    // send B's GetIntroductionProfileRequest message
                    reset { kvdbNode.publish(bRqCnxn)(bGetIntroProfileRq.toCnxnCtxtLabel, bGetIntroProfileRq.toGround) }
println("sent GetIntroductionProfileRequest on " + bRqCnxn)
                    reset {
                      // listen for B's GetIntroductionProfileResponse message
println("listen for GetIntroductionProfileResponse on " + bRspCnxn)
                      for (bgiprsp <- kvdbNode.get(
                        bRspCnxn)(
                        new GetIntroductionProfileResponse(Some(sessionId), bGetIntroProfileRq.requestId.get).toCnxnCtxtLabel)) {

                        // match response from B
                        // TODO: Get introduction profile from message
                        bgiprsp match {
                          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileResponse(_, _)))), _)) => {
println("found GetIntroductionProfileResponse on " + bRspCnxn)
                            // create A's IntroductionRequest message
                            // TODO: Add introduction profile to message
                            val aIntroRq = new IntroductionRequest(
                              Some(sessionId),
                              Some(UUID.randomUUID.toString),
                              Some(aRspCnxn), aMessage)

                            // send A's IntroductionRequest message
                            reset { kvdbNode.publish(aRqCnxn)(aIntroRq.toCnxnCtxtLabel, aIntroRq.toGround) }
println("sent IntroductionRequest on " + aRqCnxn)
                            reset {
                              // listen for A's IntroductionResponse message
println("listen for IntroductionResponse on " + aRspCnxn)
                              for (airsp <- kvdbNode.get(
                                aRspCnxn)(
                                new IntroductionResponse(Some(sessionId), aIntroRq.requestId.get).toCnxnCtxtLabel)) {

                                // match response from A
                                airsp match {
                                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                  _,
                                  _,
                                  Some(aAccepted),
                                  aCnxnName,
                                  aRejectReason,
                                  Some(aConnectId))))), _)) => {
println("found IntroductionResponse on " + aRspCnxn)
                                    // create B's IntroductionRequest message
                                    // TODO: Add introduction profile to message
                                    val bIntroRq = new IntroductionRequest(
                                      Some(sessionId),
                                      Some(UUID.randomUUID.toString),
                                      Some(bRspCnxn), bMessage)

                                    // send B's IntroductionRequest message
                                    reset { kvdbNode.publish(bRqCnxn)(bIntroRq.toCnxnCtxtLabel, bIntroRq.toGround) }
println("sent IntroductionRequest on " + bRqCnxn)
                                    reset {
                                      // listen for B's IntroductionResponse message
println("listen for IntroductionResponse on " + bRspCnxn)
                                      for (birsp <- kvdbNode.get(
                                        bRspCnxn)(
                                        new IntroductionResponse(Some(sessionId), bIntroRq.requestId.get).toCnxnCtxtLabel)) {

                                        // match response from B
                                        birsp match {
                                          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                                          _,
                                          _,
                                          Some(bAccepted),
                                          bCnxnName,
                                          bRejectReason,
                                          Some(bConnectId))))), _)) => {
println("found IntroductionResponse on " + bRspCnxn)
                                            // create BeginIntroductionResponse message
                                            val beginIntroRsp = new BeginIntroductionResponse(
                                              Some(sessionId),
                                              biRqId,
                                              Some(aAccepted && bAccepted),
                                              aRejectReason,
                                              bRejectReason)

                                            // check whether A and B accepted
                                            if (aAccepted && bAccepted) {
                                              // create new cnxns
                                              // TODO: Create new cnxns properly
                                              val aURI = new URI("cnxn://" + UUID.randomUUID().toString)
                                              val bURI = new URI("cnxn://" + UUID.randomUUID().toString)
                                              val abCnxn = new acT.AgentCnxn(aURI, "", bURI)
                                              val baCnxn = new acT.AgentCnxn(bURI, "", aURI)

                                              // create Connect messages
                                              val aConnect = new Connect(Some(sessionId), aConnectId, aCnxnName, Some(baCnxn), Some(abCnxn))
                                              val bConnect = new Connect(Some(sessionId), bConnectId, bCnxnName, Some(abCnxn), Some(baCnxn))

                                              // send Connect messages
                                              reset { kvdbNode.put(aRqCnxn)(aConnect.toCnxnCtxtLabel, aConnect.toGround) }
println("sent Connect on " + aRqCnxn)
                                              reset { kvdbNode.put(bRqCnxn)(bConnect.toCnxnCtxtLabel, bConnect.toGround) }
println("sent Connect on " + bRqCnxn)
                                              // send BeginIntroductionResponse message
                                              reset { kvdbNode.put(biRspCnxn)(beginIntroRsp.toCnxnCtxtLabel, beginIntroRsp.toGround) }
println("sent BeginIntroductionResponse on " + biRspCnxn)
                                            } else {
                                              // send BeginIntroductionResponse message
                                              reset { kvdbNode.put(biRspCnxn)(beginIntroRsp.toCnxnCtxtLabel, beginIntroRsp.toGround) }
println("sent BeginIntroductionResponse on " + biRspCnxn)
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
    cnxn: acT.AgentCnxn,
    privateReadCnxn: acT.AgentCnxn,
    privateWriteCnxn: acT.AgentCnxn,
    selfCnxn: acT.AgentCnxn) {

    reset {
      // listen for GetIntroductionProfileRequest message
println("listen for GetIntroductionProfileRequest on " + cnxn)
      for (giprq <- kvdbNode.subscribe(cnxn)(new GetIntroductionProfileRequest().toCnxnCtxtLabel)) {

        giprq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, GetIntroductionProfileRequest(
            Some(sessionId),
            Some(rqId),
            Some(rspCnxn))))), _)) => {
println("found GetIntroductionProfileRequest on " + cnxn)
            // TODO: Load introduction profile

            // create GetIntroductionProfileResponse message
            // TODO: Set introduction profile on message
            val getIntroProfileRsp = new GetIntroductionProfileResponse(Some(sessionId), rqId)

            // send GetIntroductionProfileResponse message
            Thread.sleep(1000)
            reset { kvdbNode.put(rspCnxn)(getIntroProfileRsp.toCnxnCtxtLabel, getIntroProfileRsp.toGround) }
println("sent GetIntroductionProfileResponse on " + rspCnxn)
          }
          case None => {}
          case _ => {
            // expected GetIntroductionProfileRequest
            throw new Exception("unexpected protocol message on " + cnxn)
          }
        }
      }
    }

    reset {
      // listen for IntroductionRequest message
println("listen for IntroductionRequest on " + cnxn)
      for (irq <- kvdbNode.subscribe(cnxn)(new IntroductionRequest().toCnxnCtxtLabel)) {

        irq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionRequest(
            Some(sessionId),
            Some(rqId),
            Some(rspCnxn),
            message)))), _)) => {
println("found IntroductionRequest on " + cnxn)
            // create IntroductionRequest message
            // TODO: Add introduction profile to message
            val introRq = new IntroductionRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(privateReadCnxn), message)

            // send IntroductionRequest message
            reset { kvdbNode.put(privateWriteCnxn)(introRq.toCnxnCtxtLabel, introRq.toGround) }
println("sent IntroductionRequest on " + privateWriteCnxn)
            reset {
              // listen for IntroductionResponse message
println("listen for IntroductionResponse on " + privateReadCnxn)
              for (irsp <- kvdbNode.get(
                privateReadCnxn)(
                new IntroductionResponse(Some(sessionId), introRq.requestId.get).toCnxnCtxtLabel)) {

                irsp match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, IntroductionResponse(
                  _,
                  _,
                  Some(accepted),
                  cnxnName,
                  rejectReason,
                  _)))), _)) => {
println("found IntroductionResponse on " + privateReadCnxn)
                    // create IntroductionResponse message
                    val introRsp = new IntroductionResponse(
                      Some(sessionId),
                      rqId,
                      Some(accepted),
                      cnxnName,
                      rejectReason,
                      Some(UUID.randomUUID.toString))

                    // send IntroductionResponse message
                    reset { kvdbNode.put(rspCnxn)(introRsp.toCnxnCtxtLabel, introRsp.toGround) }
println("sent IntroductionResponse on " + rspCnxn)
                    if (accepted) {
                      reset {
                        // listen for Connect message
println("listen for Connect on " + cnxn)
                        for (connect <- kvdbNode.get(
                          cnxn)(
                          new Connect(Some(sessionId), introRsp.connectId.get).toCnxnCtxtLabel)) {

                          connect match {
                            case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Connect(
                            _,
                            _,
                            Some(cnxnName),
                            Some(readCnxn),
                            Some(writeCnxn))))), _)) => {
println("found Connect on " + cnxn)
                              // TODO: Register behaviors on new request cnxn
                              reset {
                                // get the list of cnxns
println("listen for Cnxns on " + selfCnxn)
                                for (cnxns <- kvdbNode.get(selfCnxn)(new Cnxns().toCnxnCtxtLabel)) {
                                  cnxns match {
                                    case Some(mTT.RBoundHM(Some(mTT.Ground(ConcreteHL.InsertContent(_, _, Cnxns(
                                    _,
                                    cnxnList)))), _)) => {
println("found Cnxns on " + selfCnxn)
                                      // create new Cnxns object with the new cnxns
                                      val cnxns = new Cnxns(Some(new Date()), (readCnxn, writeCnxn) :: cnxnList)

                                      // save new Cnxns object
                                      reset { kvdbNode.put(selfCnxn)(cnxns.toCnxnCtxtLabel, cnxns.toGround) }
println("sent Cnxns on " + selfCnxn)
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
