package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import scala.util.continuations._

trait IntroductionRecipientT extends Serializable {
  val biCnxnsListLabel = "biCnxnsList(true)".toLabel

  private def toAgentCnxn(cnxn: PortableAgentCnxn): acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }

  def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String,String,String]]): Unit = {

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
          case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(IntroductionRequest(
            Some(sessionId),
            Some(corrId),
            Some(rspCnxn),
            message)))), _)) => {

            // create IntroductionNotification message
            // TODO: Add introduction profile to message
            val introN = new IntroductionNotification(
              Some(sessionId),
              UUID.randomUUID.toString,
              new acT.AgentBiCnxn(readCnxn, rspCnxn),
              message)

            // send IntroductionNotification message
            reset { kvdbNode.put(aliasCnxn)(introN.toCnxnCtxtLabel, introN.toGround) }

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
                            Some(newBiCnxn: acT.AgentBiCnxn))))), _)) => {

                              // TODO: Register behaviors on new request cnxn
                              reset {
                                // get the list of biCnxns
                                for (biCnxns <- kvdbNode.get(aliasCnxn)(biCnxnsListLabel)) {
                                  biCnxns match {
                                    case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr(
                                      prevBiCnxns: String))), _)) => {

                                      // add new biCnxn to the list
                                      val newBiCnxns = newBiCnxn :: Serializer.deserialize[List[acT.AgentBiCnxn]](prevBiCnxns)

                                      // save new list of biCnxns
                                      reset { kvdbNode.put(aliasCnxn)(biCnxnsListLabel, mTT.Ground(PostedExpr(Serializer.serialize(newBiCnxns)))) }

                                      // TODO: Publish Connect message to aliasCnxn
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
