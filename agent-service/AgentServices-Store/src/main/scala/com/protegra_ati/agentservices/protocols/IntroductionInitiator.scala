package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import java.util.UUID
import scala.util.continuations._

trait IntroductionInitiatorT extends Serializable {
  private def toAgentCnxn(cnxn: PortableAgentCnxn): acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }

  def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String,String,String]]): Unit = {

    if (cnxns.size != 1) throw new Exception("invalid number of cnxns supplied")

    val aliasCnxn = toAgentCnxn(cnxns(0))

    reset {
      // listen for BeginIntroductionRequest message
      for (birq <- kvdbNode.subscribe(aliasCnxn)(new BeginIntroductionRequest().toCnxnCtxtLabel)) {
        birq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(BeginIntroductionRequest(
            Some(sessionId),
            Some(acT.AgentBiCnxn(aReadCnxn, aWriteCnxn)),
            Some(acT.AgentBiCnxn(bReadCnxn, bWriteCnxn)),
            aMessage,
            bMessage)))), _)) => {
println("Got BeginIntroductionRequest: " + birq)
            // create A's GetIntroductionProfileRequest message
            val aGetIntroProfileRq = new GetIntroductionProfileRequest(
              Some(sessionId),
              Some(UUID.randomUUID.toString),
              Some(aReadCnxn))

            // send A's GetIntroductionProfileRequest message
            reset { kvdbNode.publish(aWriteCnxn)(aGetIntroProfileRq.toCnxnCtxtLabel, aGetIntroProfileRq.toGround) }
println("Sent A's GetIntroductionProfileRequest: " + aGetIntroProfileRq)
            reset {
              // listen for A's GetIntroductionProfileResponse message
              for (agiprsp <- kvdbNode.get(
                aReadCnxn)(
                new GetIntroductionProfileResponse(Some(sessionId), aGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel)) {
println("Got A's GetIntroductionProfileResponse: " + agiprsp)
                // match response from A
                agiprsp match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(GetIntroductionProfileResponse(
                    _,
                    _,
                    Some(aProfileData))))), _)) => {

                    // create B's GetIntroductionProfileRequest message
                    val bGetIntroProfileRq = new GetIntroductionProfileRequest(
                      Some(sessionId),
                      Some(UUID.randomUUID.toString),
                      Some(bReadCnxn))

                    // send B's GetIntroductionProfileRequest message
                    reset { kvdbNode.publish(bWriteCnxn)(bGetIntroProfileRq.toCnxnCtxtLabel, bGetIntroProfileRq.toGround) }
println("Sent B's GetIntroductionProfileRequest: " + bGetIntroProfileRq)
                    reset {
                      // listen for B's GetIntroductionProfileResponse message
                      for (bgiprsp <- kvdbNode.get(
                        bReadCnxn)(
                        new GetIntroductionProfileResponse(Some(sessionId), bGetIntroProfileRq.correlationId.get).toCnxnCtxtLabel)) {

                        // match response from B
                        bgiprsp match {
                          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(GetIntroductionProfileResponse(
                            _,
                            _,
                            Some(bProfileData))))), _)) => {

                            // create A's IntroductionRequest message
                            val aIntroRq = new IntroductionRequest(
                              Some(sessionId),
                              Some(UUID.randomUUID.toString),
                              Some(aReadCnxn),
                              aMessage,
                              Some(bProfileData))
println("Got B's GetIntroductionProfileResponse: " + bgiprsp)
                            // send A's IntroductionRequest message
                            reset { kvdbNode.publish(aWriteCnxn)(aIntroRq.toCnxnCtxtLabel, aIntroRq.toGround) }
println("Sent A's IntroductionRequest: " + aIntroRq)
                            reset {
                              // listen for A's IntroductionResponse message
                              for (airsp <- kvdbNode.get(
                                aReadCnxn)(
                                new IntroductionResponse(Some(sessionId), aIntroRq.correlationId.get).toCnxnCtxtLabel)) {

                                // match response from A
                                airsp match {
                                  case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(IntroductionResponse(
                                  _,
                                  _,
                                  Some(aAccepted),
                                  Some(aConnectId))))), _)) => {
println("Got A's IntroductionResponse: " + airsp)
                                    // create B's IntroductionRequest message
                                    val bIntroRq = new IntroductionRequest(
                                      Some(sessionId),
                                      Some(UUID.randomUUID.toString),
                                      Some(bReadCnxn),
                                      bMessage,
                                      Some(aProfileData))

                                    // send B's IntroductionRequest message
                                    reset { kvdbNode.publish(bWriteCnxn)(bIntroRq.toCnxnCtxtLabel, bIntroRq.toGround) }
println("Sent B's IntroductionRequest: " + bIntroRq)
                                    reset {
                                      // listen for B's IntroductionResponse message
                                      for (birsp <- kvdbNode.get(
                                        bReadCnxn)(
                                        new IntroductionResponse(Some(sessionId), bIntroRq.correlationId.get).toCnxnCtxtLabel)) {

                                        // match response from B
                                        birsp match {
                                          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(IntroductionResponse(
                                          _,
                                          _,
                                          Some(bAccepted),
                                          Some(bConnectId))))), _)) => {
println("Got B's IntroductionResponse: " + birsp)
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
                                              Thread.sleep(1000)
                                              reset { kvdbNode.put(aWriteCnxn)(aConnect.toCnxnCtxtLabel, aConnect.toGround) }
                                              reset { kvdbNode.put(bWriteCnxn)(bConnect.toCnxnCtxtLabel, bConnect.toGround) }
                                            } else if (aAccepted) {
                                              // create Connect message
                                              val aConnect = new Connect(Some(sessionId), aConnectId, Some(true), None)

                                              // send Connect message
                                              Thread.sleep(1000)
                                              reset { kvdbNode.put(aWriteCnxn)(aConnect.toCnxnCtxtLabel, aConnect.toGround) }
                                            } else if (bAccepted) {
                                              // create Connect message
                                              val bConnect = new Connect(Some(sessionId), bConnectId, Some(true), None)

                                              // send Connect message
                                              Thread.sleep(1000)
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
}

class IntroductionInitiator extends IntroductionInitiatorT {
  override def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String,String,String]]): Unit = {

    super.run(kvdbNode, cnxns, filters)
  }
}
