package com.biosimilarity.lift.test

/**
  * lifted from SimpleMsg.scala
  */
trait AMQPTestUtility[A] {

  trait Message {
    def a: A
    def i: Int
    def b: Boolean
    def r: Option[Message]
  }

  case class Msg(a: A, i: Int, b: Boolean, r: Option[Message]) extends Message

  def msgStreamPayload(idx: Int): A

  var _msgStrm: Option[Stream[Message]] = None

  def msgStream[M <: Message](mkMsg: (Boolean, Int, A, Option[Message]) => M): Stream[M] = {
    lazy val msgStrm: Stream[M] =
      List(mkMsg(true, 0, msgStreamPayload(0), None)).toStream append {
        msgStrm.map { msg =>
          val j = msg.i + 1
          mkMsg(((j % 2) == 0), j, msgStreamPayload(j), Some(msg))
        }
      }
    _msgStrm = Some(msgStrm)
    msgStrm
  }

  def msgStream: Stream[Message] = _msgStrm match {
    case Some(msgStrm) => msgStrm
    case None          => msgStream((b: Boolean, i: Int, a: A, r: Option[Message]) => Msg(a, i, b, r))
  }
}
