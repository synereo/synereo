package com.biosimilarity.evaluator.spray

import akka.actor.{Actor, ActorRef}
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper

import scala.concurrent.duration._

object SessionManagerActor extends Serializable {
  case class InitSession(actor: ActorRef)                extends Serializable
  case class SetDefaultSessionTimeout(t: FiniteDuration) extends Serializable
  case class SetDefaultPongTimeout(t: FiniteDuration)    extends Serializable
}

class SessionManagerActor extends Actor with Serializable {

  import SessionManagerActor._

  // if client doesnt receive any messages within this time, its garbage collected
  @transient
  var defaultSessionTimeout = EvalConfigWrapper.readInt("sessionTimeoutMinutes") minutes

  // ping requests are ponged after this much time unless other data is sent in the meantime
  // clients need to re-ping after this
  @transient
  var defaultPongTimeout = EvalConfigWrapper.readInt("pongTimeoutSeconds") seconds

  def receive = {
    case SetDefaultSessionTimeout(t) =>
      defaultSessionTimeout = t

    case SetDefaultPongTimeout(t) =>
      defaultPongTimeout = t

    case InitSession(actor: ActorRef) =>
      actor ! SessionActor.SetSessionTimeout(defaultSessionTimeout)
      actor ! SessionActor.SetPongTimeout(defaultPongTimeout)
  }

  SessionManager.setSessionManager(context)
}
