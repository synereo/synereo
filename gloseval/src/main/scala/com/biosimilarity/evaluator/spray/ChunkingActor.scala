package com.biosimilarity.evaluator.spray

import akka.actor.{Actor, ActorRef}

object ChunkingActor extends Serializable {
  case class SetChunkSize(sz: Int)       extends Serializable
  case class SetExpected(n: Int)         extends Serializable
  case class SetRecipient(ref: ActorRef) extends Serializable
}

class ChunkingActor extends Actor with Serializable {

  import ChunkingActor._

  @transient
  var chunkSize = 20

  @transient
  var expected = -1

  @transient
  var recipient: Option[ActorRef] = None

  @transient
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
