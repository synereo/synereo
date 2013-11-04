package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.lib.BasicLogService
import com.protegra_ati.agentservices.msgs.agent.introduction._
import com.protegra_ati.agentservices.protocols.msgs._
import java.util.UUID
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

trait AgentIntroductionSchema extends AgentCRUDSchema {
  self : EvaluationCommsService =>
}

trait AgentIntroductionHandler extends AgentIntroductionSchema {
  self : EvaluationCommsService =>

  import DSLCommLink.mTT

  //### Introduction Protocol
  //#### beginIntroduction
  def handlebeginIntroductionRequest(
    key : String,
    msg : beginIntroductionRequest
    ) : Unit = {
    BasicLogService.tweet( "Entering: handlebeginIntroductionRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )
    val sessionId = UUID.randomUUID().toString

    val birq = new BeginIntroductionRequest(
      Some( sessionId ),
      Some( toAgentBiCnxn( msg.aConnection ) ),
      Some( toAgentBiCnxn( msg.bConnection ) ),
      Some( msg.aMessage ),
      Some( msg.bMessage )
    )

    val onPost : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlebeginIntroductionRequest | onPost" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
        ( "msgType" -> "beginIntroductionResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    val specimen = MessageConverter.beginIntroductionRequestToSpecimen(birq)
    agentMgr().postV( birq.toCnxnCtxtLabel, List( aliasStorageCnxn ), specimen, onPost )
  }

  private def toAgentBiCnxn( cnxn : PortableAgentCnxn ) : acT.AgentBiCnxn = {
    new acT.AgentBiCnxn(
      new acT.AgentCnxn( cnxn.trgt, cnxn.label, cnxn.src ),
      new acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt )
    )
  }
}
