package com.biosimilarity.evaluator.spray

import akka.actor.{Actor, ActorRef}

object ChunkingActor extends Serializable {
  case class SetChunkSize(sz: Int)       extends Serializable
  case class SetExpected(n: Int)         extends Serializable
}

class ChunkingActor(recipient: ActorRef) extends Actor with Serializable {

  import ChunkingActor._

  @transient
  var chunkSize = 20

  @transient
  var expected = -1

  @transient
  var msgs: List[String] = Nil

  def receive = {

    case SetChunkSize(sz) => chunkSize = sz

    case SetExpected(sz) => expected = sz

    case SessionActor.CometMessage(data) =>
      msgs = data :: msgs
      expected -= 1
      if (msgs.length == chunkSize || expected == 0) {
        recipient ! SessionActor.CometMessageList(msgs.reverse)
        msgs = Nil
        if (expected == 0) context.stop(self)
      }
  }
}
