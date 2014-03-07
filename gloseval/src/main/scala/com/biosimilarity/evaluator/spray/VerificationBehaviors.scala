package com.biosimilarity.evaluator.spray

import scala.concurrent.FJTaskRunnersX

trait VerificationBehaviorsT extends FJTaskRunnersX {
  import com.protegra_ati.agentservices.protocols._
  import com.protegra_ati.agentservices.protocols.msgs.CompleteClaim
  import com.protegra_ati.agentservices.protocols.msgs.VerificationNotification

  import com.biosimilarity.evaluator.distribution._
  import com.biosimilarity.evaluator.distribution.ConcreteHL._
  import com.biosimilarity.evaluator.distribution.DSLCommLink.mTT
  import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext._
  import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext.eServe._

  import com.biosimilarity.lift.lib.BasicLogService
  
  import com.biosimilarity.lift.model.store._

  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.JsonDSL._

  import java.net.URI
  import java.util.UUID

  def launchClaimantBehavior(
    selfURI: URI,
    feed: (CnxnCtxtLabel[String,String,String], Seq[ConcreteHL.Cnxn], Option[mTT.Resource] => Unit) => Unit,
    fail: String => Unit = (s: String) => BasicLogService.tweet(s)
  ): Unit = {
    val doCompleteClaim = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet("doCompleteClaim: optRsrc = " + optRsrc)
      optRsrc match {
        case None => ()
        case Some(mTT.RBoundHM(Some(mTT.Ground(Bottom)), _)) => ()
        case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr((PostedExpr(CompleteClaim(
          sessionId,
          correlationId,
          verifier,
          claim,
          data
        )), _, _, _)))), _)) => {
          CometActorMapper.cometMessage(sessionId, compact(render(
            ("msgType" -> "completeClaim") ~
              ("content" -> (
                ("sessionURI" -> sessionId) ~
                ("correlationId" -> correlationId) ~
                ("verifier" -> (
                  ("src" -> verifier.src.toString) ~
                  ("label" -> verifier.label) ~
                  ("tgt" -> verifier.trgt.toString)
                )) ~
                ("claim" -> claim.toString.replace("'", "")) ~
                ("data" -> data.toString.replace("'",""))
              ))
          )))
        }
        case _ => fail("launchClaimantBehavior -- Unrecognized resource: " + optRsrc)
      }
    }

    val pacSelfToGlos = PortableAgentCnxn(selfURI, "verificationProtocol", new URI("ui://gloseval"))
    val pacGlosToSelf = PortableAgentCnxn(new URI("ui://gloseval"), "verificationProtocol", selfURI)
    feed(
      CompleteClaim.toLabel(),
      List(pacGlosToSelf),
      doCompleteClaim
    )
    bFactoryMgr().commenceInstance(
      verifierProtocolCnxn,
      claimantLabel,
      List(pacSelfToGlos),
      Nil,
      optRsrc => println( "onCommencement claimant: optRsrc = " + optRsrc )
    )
  }

  def launchVerificationAndRelyingPartyBehaviors(
    selfURI: URI,
    claimantURI: URI,
    feed: (CnxnCtxtLabel[String,String,String], Seq[ConcreteHL.Cnxn], Option[mTT.Resource] => Unit) => Unit,
    fail: String => Unit = (s: String) => BasicLogService.tweet(s)
  ): Unit = {
    val doVerificationNotification = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet("doVerificationNotification: optRsrc = " + optRsrc)
      optRsrc match {
        case None => ()
        case Some(mTT.RBoundHM(Some(mTT.Ground(Bottom)), _)) => ()
        case Some(mTT.RBoundHM(Some(mTT.Ground(PostedExpr((PostedExpr(VerificationNotification(
          sessionId,
          correlationId,
          claimant,
          claim,
          witness
        )), _, _, _)))), _)) => {
          CometActorMapper.cometMessage(sessionId, compact(render(
            ("msgType" -> "verificationNotification") ~
              ("content" -> (
                ("sessionURI" -> sessionId) ~
                ("correlationId" -> correlationId) ~
                ("claimaint" -> (
                  ("src" -> claimant.src.toString) ~
                  ("label" -> claimant.label) ~
                  ("tgt" -> claimant.trgt.toString)
                )) ~
                ("claim" -> claim.toString.replace("'", "")) ~
                ("witness" -> witness.toString.replace("'", ""))
              ))
          )))
        }
        case _ => fail("launchVerificationAndRelyingPartyBehaviors -- Unrecognized resource: " + optRsrc)
      }
    }

    val pacSelfToGlos = PortableAgentCnxn(selfURI, "verificationProtocol", new URI("ui://gloseval"))

    //val claimantToSelfCnxnTitle = UUID.randomUUID.toString.substring(0,8)
    // BUGBUG : lgm -- find a better way to do this
    val claimantToSelfCnxnTitle = "claimantToSelf"

    val pacClaimantToSelf = PortableAgentCnxn(claimantURI, claimantToSelfCnxnTitle, selfURI)
    val pacSelfToClaimant = PortableAgentCnxn(selfURI, claimantToSelfCnxnTitle, claimantURI)

    // TODO(mike): create two different subscriptions, one for relying party and one for verifier
    feed(
      VerificationNotification.toLabel(),
      List(pacSelfToClaimant),
      doVerificationNotification
    )
    bFactoryMgr().commenceInstance(
      verifierProtocolCnxn,
      relyingPartyLabel,
      List(pacSelfToGlos, pacClaimantToSelf),
      Nil,
      optRsrc => println( "onCommencement relyingParty: optRsrc = " + optRsrc )
    )
    bFactoryMgr().commenceInstance(
      verifierProtocolCnxn,
      verifierLabel,
      List(pacSelfToGlos, pacClaimantToSelf),
      Nil,
      optRsrc => println( "onCommencement verifier: optRsrc = " + optRsrc )
    )
  }
}

case class VerificationBehaviors() extends VerificationBehaviorsT
