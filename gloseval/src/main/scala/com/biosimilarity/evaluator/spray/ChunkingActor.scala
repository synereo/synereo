package com.biosimilarity.evaluator.spray

import akka.actor.{Actor, ActorRef}

object ChunkingActor {
  case class SetChunkSize(sz: Int)
  case class SetExpected(n: Int)
  case class SetRecipient(ref: ActorRef)
}

class ChunkingActor extends Actor {

  import ChunkingActor._

  var chunkSize                   = 20
  var expected                    = -1
  var recipient: Option[ActorRef] = None

  var msgs: List[String] = Nil
  def receive = {
    case SetRecipient(ref) => recipient = Some(ref)

    case SetChunkSize(sz) => chunkSize = sz

    case SetExpected(sz) => expected = sz

    case SessionActor.CometMessage(data) =>
      msgs = data :: msgs
      expected -= 1
      if (msgs.length == chunkSize || expected == 0) {
        recipient match {
          case Some(ref) =>
            ref ! SessionActor.CometMessageList(msgs.reverse)
            msgs = Nil
            if (expected == 0) context.stop(self)

          case None =>
            throw new Exception("Recipient not set for chunking actor")
        }
      }
  }
}
