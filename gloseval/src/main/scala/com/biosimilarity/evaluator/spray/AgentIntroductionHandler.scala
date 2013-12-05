package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution._
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
    msg : beginIntroductionRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlebeginIntroductionRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )
    val sessionId = UUID.randomUUID().toString

    val birq = new BeginIntroductionRequest(
      sessionId,
      toBiCnxn( msg.aConnection ),
      toBiCnxn( msg.bConnection ),
      Some( msg.aMessage ),
      Some( msg.bMessage )
    )

    val onPost : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlebeginIntroductionRequest | onPost" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage(sessionURIStr, compact( render(
        ( "msgType" -> "beginIntroductionResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    agentMgr().post( birq.toLabel, List( aliasStorageCnxn ), birq, onPost )
  }

  //#### introductionConfirmation
  def handleintroductionConfirmationRequest(
    msg : introductionConfirmationRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleintroductionConfirmationRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )
    val introConfirmation = new IntroductionConfirmation( msg.introSessionId, msg.correlationId, msg.accepted )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handleintroductionConfirmationRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage(sessionURIStr, compact( render(
        ( "msgType" -> "introductionConfirmationResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    agentMgr().put( introConfirmation.toLabel, List( aliasStorageCnxn ), introConfirmation, onPut )

    // removes introduction notification
    val introNotificationLabel = IntroductionNotification.toLabel(msg.introSessionId)
    agentMgr().get( introNotificationLabel, List ( aliasStorageCnxn ) )
  }

  private def toBiCnxn( cnxn : PortableAgentCnxn ) : PortableAgentBiCnxn = {
    PortableAgentBiCnxn( PortableAgentCnxn( cnxn.trgt, cnxn.label, cnxn.src ), cnxn )
  }
}
