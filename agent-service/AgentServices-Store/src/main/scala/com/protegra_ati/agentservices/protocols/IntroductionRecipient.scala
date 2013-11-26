package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import scala.util.continuations._

trait IntroductionRecipientT extends Serializable {
  val biCnxnsListLabel = "biCnxnsList(true)".toLabel
  val profileDataLabel = "jsonBlob(W)".toLabel

  private def toAgentCnxn(cnxn: PortableAgentCnxn): acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }

  def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]): Unit = {

    if (cnxns.size != 2) throw new Exception("invalid number of cnxns supplied")

    val readCnxn = toAgentCnxn(cnxns(0))
    val aliasCnxn = toAgentCnxn(cnxns(1))

    reset {
      // listen for GetIntroductionProfileRequest message
      for (giprq <- kvdbNode.subscribe(readCnxn)(new GetIntroductionProfileRequest().toCnxnCtxtLabel)) {

        giprq match {
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(GetIntroductionProfileRequest(
            Some(sessionId),
            Some(corrId),
            Some(rspCnxn))))), _)) => {

            // TODO: get data from aliasCnxn once it is being stored there
            val identityCnxn = acT.AgentCnxn(aliasCnxn.src, "identity", aliasCnxn.trgt)

            reset {
              // get the profile data
              for (e <- kvdbNode.read(identityCnxn)(profileDataLabel)) {
                e match {
                  case Some(mTT.Ground(PostedExpr(jsonBlob: String))) => {
                    // create GetIntroductionProfileResponse message
                    val getIntroProfileRsp = new GetIntroductionProfileResponse(Some(sessionId), corrId, Some(jsonBlob))

                    // send GetIntroductionProfileResponse message
                    Thread.sleep(1000)
                    reset { kvdbNode.put(rspCnxn)(getIntroProfileRsp.toCnxnCtxtLabel, getIntroProfileRsp.toGround) }
                  }
                  case _ => {
                    // expected String of profile data
                    throw new Exception("unexpected profile data")
                  }
                }
              }
            }
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
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(IntroductionRequest(
            Some(sessionId),
            Some(corrId),
            Some(rspCnxn),
            message,
            Some(profileData))))), _)) => {

            // create IntroductionNotification message
            val introN = new IntroductionNotification(
              Some(sessionId),
              UUID.randomUUID.toString,
              new acT.AgentBiCnxn(readCnxn, rspCnxn),
              message,
              profileData)

            // send IntroductionNotification message
            reset { kvdbNode.publish(aliasCnxn)(introN.toCnxnCtxtLabel, introN.toGround) }

            reset {
              // listen for IntroductionConfirmation message
              for (ic <- kvdbNode.get(
                aliasCnxn)(
                new IntroductionConfirmation(Some(sessionId), introN.correlationId).toCnxnCtxtLabel)) {

                ic match {
                  case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(IntroductionConfirmation(
                  _,
                  _,
                  Some(accepted))))), _)) => {

                    // create IntroductionResponse message
                    val introRsp = new IntroductionResponse(
                      Some(sessionId),
                      corrId,
                      Some(accepted),
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
                            case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(Connect(
                            _,
                            _,
                            Some(newBiCnxn: PortableAgentBiCnxn))))), _)) => {

                              // TODO: Register behaviors on new request cnxn
                              reset {
                                // get the list of biCnxns
                                for (biCnxns <- kvdbNode.get(aliasCnxn)(biCnxnsListLabel)) {
                                  biCnxns match {
                                    case Some(mTT.Ground(PostedExpr(prevBiCnxns: String))) => {
                                      // add new biCnxn to the list
                                      val newBiCnxns = newBiCnxn :: Serializer.deserialize[List[PortableAgentBiCnxn]](prevBiCnxns)

                                      // serialize biCnxn list
                                      val newBiCnxnsStr = Serializer.serialize(newBiCnxns)

                                      // save new list of biCnxns
                                      reset { kvdbNode.put(aliasCnxn)(biCnxnsListLabel, mTT.Ground(PostedExpr(newBiCnxnsStr))) }

                                      // create ConnectNotification message
                                      val connectN = new ConnectNotification(Some(sessionId), newBiCnxn, profileData)

                                      // send ConnectNotification message
                                      reset { kvdbNode.publish(aliasCnxn)(connectN.toCnxnCtxtLabel, connectN.toGround) }
                                    }
                                    case _ => {
                                      // expected String of serialized biCnxns
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

class IntroductionRecipient extends IntroductionRecipientT
