package com.biosimilarity.lift.test

import com.biosimilarity.lift.model.store.MonadicJSONFramedMsgDispatcher

/**
  * lifted from MonadicNode.scala
  */
class MonadicNodeUsage {

  import java.net.URI

  import com.biosimilarity.lift.lib.BasicLogService
  import com.biosimilarity.lift.lib.moniker._
  import com.biosimilarity.lift.model.agent._
  import com.biosimilarity.lift.model.msg._

  import scala.collection.mutable
  import scala.util.continuations._

  object MsgStreamFactory extends AMQPTestUtility[String] with Serializable {
    override def msgStreamPayload(idx: Int): String = { "Msg" + idx }
  }

  object FramedMsgDispatcherUseCase extends Serializable {

    trait UseCaseProtocol extends MsgStreamFactory.Message

    trait UseCaseRequest extends UseCaseProtocol

    trait UseCaseResponse extends UseCaseProtocol

    case class UseCaseRequestOne(b: Boolean, i: Int, a: String, r: Option[MsgStreamFactory.Message]) extends UseCaseRequest

    case class UseCaseResponseOne(b: Boolean, i: Int, a: String, r: Option[MsgStreamFactory.Message]) extends UseCaseResponse

    case class FramedUseCaseProtocolDispatcher(here: URI, there: URI)
        extends MonadicJSONFramedMsgDispatcher[UseCaseRequest, UseCaseResponse](
          Individual(MURI(here),
                     new mutable.ListBuffer[JustifiedRequest[UseCaseRequest, UseCaseResponse]](),
                     new mutable.ListBuffer[JustifiedResponse[UseCaseRequest, UseCaseResponse]]()),
          List[Moniker](MURI(there))) {
      override def toString(): String = {
        "FramedUseCaseProtocolDispatcher" + "[" + here + " -> " + there + "]"
      }
    }

    type FUCPD = FramedUseCaseProtocolDispatcher

    implicit val retTwist: Boolean = false

    def setup(localHost: String, localPort: Int, remoteHost: String, remotePort: Int)(
        implicit returnTwist: Boolean): Either[FUCPD, (FUCPD, FUCPD)] = {

      val (localExchange, remoteExchange) = if (localHost.equals(remoteHost) && (localPort == remotePort)) {
        ("/useCaseProtocolLocal", "/useCaseProtocolRemote")
      } else {
        ("/useCaseProtocol", "/useCaseProtocol")
      }

      val localURI  = new URI("agent", null, localHost, localPort, localExchange, null, null)
      val remoteURI = new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null)

      if (returnTwist)
        Right[FUCPD, (FUCPD, FUCPD)](
          (FramedUseCaseProtocolDispatcher(localURI, remoteURI), FramedUseCaseProtocolDispatcher(remoteURI, localURI)))
      else
        Left[FUCPD, (FUCPD, FUCPD)](FramedUseCaseProtocolDispatcher(localURI, remoteURI))
    }

    implicit val numberOfMsgs: Int = 100

    def runClient(dispatcher: FUCPD)(implicit numMsgs: Int): Unit = {

      type OM = Option[MsgStreamFactory.Message]

      val reqs = MsgStreamFactory
        .msgStream[UseCaseRequest] { (b: Boolean, i: Int, a: String, r: OM) =>
          UseCaseRequestOne(b, i, a, r)
        }
        .take(numMsgs)
        .toList

      // map-reduce-style protocol checking

      val msgMap = new mutable.HashMap[Int, Either[UseCaseRequest, (UseCaseRequest, UseCaseResponse)]]()

      msgMap += (0 -> Left[UseCaseRequest, (UseCaseRequest, UseCaseResponse)](reqs(0)))
      dispatcher ! reqs(0)

      val thr: Thread = new Thread {
        override def run(): Unit = reset {
          for (msg <- dispatcher ? ()) {
            BasicLogService.tweet(dispatcher + " received: " + msg)
            msg match {
              case Right(rsp @ UseCaseResponseOne(b, j, a, r)) =>
                BasicLogService.tweet(dispatcher + " handling response for the " + j + "th " + "request")
                msgMap.get(j) match {
                  case Some(Left(req)) =>
                    msgMap += (j -> Right[UseCaseRequest, (UseCaseRequest, UseCaseResponse)]((req, rsp)))
                    if (j < (numMsgs - 1)) {
                      // Open with left brace ...
                      msgMap += ((j + 1) -> Left[UseCaseRequest, (UseCaseRequest, UseCaseResponse)](reqs(j + 1)))
                      dispatcher ! reqs(j + 1)
                    } else
                      BasicLogService.tweet("Client side of test complete.")
                  case unExpected @ _ =>
                    throw new Exception("Protocol violated. Received: " + unExpected)
                }
              case unExpected @ _ =>
                throw new Exception("Protocol violated. Received: " + unExpected)
            }
          }
        }
      }

      thr.start()
    }

    def runServer(dispatcher: FUCPD)(implicit numMsgs: Int): Unit = {
      // map-reduce-style protocol checking
      val msgMap = new mutable.HashMap[Int, Either[UseCaseRequest, (UseCaseRequest, UseCaseResponse)]]()
      val thr: Thread = new Thread {
        override def run(): Unit = reset {
          for (msg <- dispatcher ? ()) {
            BasicLogService.tweet(dispatcher + " received: " + msg)
            msg match {
              case Left(req @ UseCaseRequestOne(b, j, a, r)) =>
                BasicLogService.tweet(dispatcher + " handling the " + j + "th " + "request")
                if (j < numMsgs) {
                  msgMap.get(j) match {
                    case None =>
                      val rsp = UseCaseResponseOne(b, j, a, Some(req))
                      msgMap +=
                        (j -> Right[UseCaseRequest, (UseCaseRequest, UseCaseResponse)](req, rsp))
                      dispatcher !! rsp
                      if (msgMap.size == numMsgs) {
                        BasicLogService.tweet("Server side of test complete.")
                      }
                    case unExpected @ _ =>
                      throw new Exception("Protocol violated. Received: " + unExpected)
                  }
                } else
                  BasicLogService.tweet("Server side of test complete.")
              case unExpected @ _ =>
                throw new Exception("Protocol violated. Received: " + unExpected)
            }
          }
        }
      }
      thr.start()
    }
  }
}
