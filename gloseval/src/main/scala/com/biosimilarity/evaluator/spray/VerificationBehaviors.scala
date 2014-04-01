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

      def handleCompleteClaim( v : ConcreteHL.HLExpr ) : Unit = {
        v match {
          case PostedExpr((PostedExpr(CompleteClaim( sessionId, correlationId, verifier, claim, data )), _, _, _)) => {
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
                ("claim" -> claim.show) ~
                ("data" -> data.show)
              ))
            )))
          }
          case _ => {
            fail("launchClaimantBehavior -- Unrecognized response: " + v)
          }
        }        
      }

      optRsrc match {
        case None => ();
        // colocated
        case Some(mTT.Ground(Bottom)) => ();
        // distributed
        case Some(mTT.RBoundHM(Some(mTT.Ground(Bottom)), _)) => ();
        // colocated
        case Some(mTT.Ground( v )) => {
          handleCompleteClaim( v )
        }
        // distributed
        case Some(mTT.RBoundHM(Some(mTT.Ground( v )), _)) => {
          handleCompleteClaim( v )
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
    //bFactoryMgr().commenceInstance(
    commenceInstance(
      verifierProtocolCnxn,
      claimantLabel,
      List(pacSelfToGlos),
      Nil,
      //optRsrc => println( "onCommencement claimant: optRsrc = " + optRsrc )
      optRsrc => BasicLogService.tweet( "onCommencement claimant: optRsrc = " + optRsrc )
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
      
      def handleVerificationNotification( v : ConcreteHL.HLExpr ) : Unit = {
        v match {
          case PostedExpr((PostedExpr(VerificationNotification( sessionId, correlationId, claimant, claim, witness )), _, _, _)) => {
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
                ("claim" -> claim.show) ~
                ("witness" -> witness.show)
              ))
            )))
          }
          case _ => {
            fail("launchVerificationAndRelyingPartyBehaviors -- Unrecognized response: " + v)
          }
        }        
      }

      optRsrc match {
        case None => ();
        // colocated
        case Some(mTT.Ground(Bottom)) => ();
        // distributed
        case Some(mTT.RBoundHM(Some(mTT.Ground(Bottom)), _)) => ();
        // either colocated or distributed
        case Some(mTT.RBoundHM(Some(mTT.Ground( v )), _)) => {
          handleVerificationNotification( v )
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
    //bFactoryMgr().commenceInstance(
    commenceInstance(
      verifierProtocolCnxn,
      relyingPartyLabel,
      List(pacSelfToGlos, pacClaimantToSelf),
      Nil,
      //optRsrc => println( "onCommencement relyingParty: optRsrc = " + optRsrc )
      optRsrc => BasicLogService.tweet( "onCommencement relyingParty: optRsrc = " + optRsrc )
    )
    //bFactoryMgr().commenceInstance(
    commenceInstance(
      verifierProtocolCnxn,
      verifierLabel,
      List(pacSelfToGlos, pacClaimantToSelf),
      Nil,
      //optRsrc => println( "onCommencement verifier: optRsrc = " + optRsrc )
      optRsrc => BasicLogService.tweet( "onCommencement verifier: optRsrc = " + optRsrc )
    )
  }
}

case class VerificationBehaviors() extends VerificationBehaviorsT
