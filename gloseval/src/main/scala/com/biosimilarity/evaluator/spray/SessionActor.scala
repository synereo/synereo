package com.biosimilarity.evaluator.spray

import java.net.URI

import akka.actor.{Actor, Cancellable}
import com.biosimilarity.evaluator.spray.wallet.{DeterministicSeedData, WalletProvider}
import org.bitcoinj.core.{Address, Coin, InsufficientMoneyException, Transaction}
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.wallet.Wallet
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.{DefaultFormats, _}
import spray.http.{DateTime, HttpResponse, StatusCodes}
import spray.routing.RequestContext

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object SessionActor {
  case class CameraItem(sending: Boolean, msgType: String, content: Option[JObject], tmStamp: DateTime)
  case class CloseSession(reqCtx: Option[RequestContext])
  case class CometMessage(data: String)
  case class CometMessageList(data: List[String])
  case class RunFunction(fn: JObject => Unit, msgType: String, content: JObject)
  case class SessionPing(reqCtx: RequestContext)
  case class SetPongTimeout(t: FiniteDuration)
  case class SetSessionId(id: String)
  case class SetSessionTimeout(t: FiniteDuration)
  case class StartCamera(reqCtx: RequestContext)
  case class StopCamera(reqCtx: RequestContext)
  case class StartWalletKit(seedData: String)
  case class SendAmps(address: Address, coin: Coin, kit: WalletAppKit, ctx: RequestContext)
  case object PongTimeout
  case object SessionTimedOut
}

class SessionActor(sessionId: String) extends Actor {

  import SessionActor._

  // if client doesnt receive any messages within this time, its garbage collected
  var sessionTimeout = 60.minutes // initial value will be overwritten during instantiation

  // ping requests are ponged after this much time unless other data is sent in the meantime
  // clients need to re-ping after this
  var pongTimeout = 7.seconds // initial value will be overwritten during instantiation

  var aliveTimer: Cancellable                       = new Cancellable { def cancel() = {}; def isCancelled = true }
  var optReq: Option[(RequestContext, Cancellable)] = None
  var msgs: List[String]                            = Nil
  var camera: Option[List[CameraItem]]              = None

  implicit val formats = DefaultFormats

  def resetAliveTimer(): Unit = {
    aliveTimer.cancel()
    aliveTimer = context.system.scheduler.scheduleOnce(sessionTimeout, self, SessionTimedOut)
  }

  def itemToString(itm: CameraItem): String = {
    val cont: JValue = itm.content match {
      case Some(j) => j
      case None    => JNull

    }
    val jo: JObject =
      ("msgType"     -> itm.msgType) ~
        ("direction" -> (if (itm.sending) "Sent" else "Received")) ~
        ("tmStamp"   -> itm.tmStamp.toIsoDateTimeString) ~
        ("content"   -> cont)
    pretty(render(jo))
  }

  def addCameraItem(itm: CameraItem): Unit = {
    camera match {
      case Some(l) => camera = Some(itm :: l)
      case None    => ()
    }
  }

  def itemReceived(msgType: String, content: Option[JObject]): Unit = {
    if (camera.isDefined) addCameraItem(CameraItem(sending = false, msgType, content, DateTime.now))
  }

  def itemSent(msgType: String, content: Option[JObject]): Unit = {
    if (camera.isDefined) addCameraItem(CameraItem(sending = true, msgType, content, DateTime.now))
  }

  def sendMessages(msgs: List[String], reqCtx: RequestContext): Unit = {
    val now = DateTime.now
    if (camera.isDefined) {
      msgs.foreach(msg => {
        implicit val formats = DefaultFormats

        val jo      = parse(msg)
        val msgType = (jo \ "msgType").extract[String]
        val content = (jo \ "content").extract[JObject]
        addCameraItem(CameraItem(sending = true, msgType, Some(content), now))
      })
    }
    msgs.foreach(msg => {
      if(msg.contains("balanceChanged")) {
        println(s"-----------------------------> Sending BALANCE_CHANGED message: $msg")
      }
      if(msg.contains("downloadComplete")) {
        println(s"-----------------------------> Sending DOWNLOAD_COMPLETE message: $msg")
      }
    })
    reqCtx.complete(HttpResponse(entity = "[" + msgs.mkString(",") + "]"))
  }

  def trySendMessages(): Unit = {
    optReq match {
      case None => () //println("CometMessage optReqCtx miss: id = " + sessionId)

      case Some((reqCtx, tmr)) => {
        tmr.cancel()
        sendMessages(msgs.reverse, reqCtx)
        optReq = None
        msgs = Nil
      }
    }
  }

  def sendCoins(address: Address, coin: Coin, kit: WalletAppKit, ctx: RequestContext) = {
    def completeWithError(errMsg: String) = {
      println(errMsg)
      ctx.complete(StatusCodes.OK, compact(render(
        ("msgType" -> "sendAmpsError") ~
          ("content" ->
            ("reason" -> errMsg)
            ))))
    }
    try {
      val result: Wallet.SendResult = kit.wallet.sendCoins(kit.peerGroup, address, coin)
      println("coins sent. transaction hash: " + result.tx.getHashAsString)
      result.broadcastComplete.get()
      kit.wallet().addWatchedAddress(address)
      ctx.complete(StatusCodes.OK, compact(render(
        ("msgType" -> "sendAmpsResponse") ~
          ("content" ->
            ("sessionURI" -> sessionId) ~
            ("transaction" -> result.tx.getHashAsString)))))
    }
    catch {
      case e: InsufficientMoneyException =>
        completeWithError("Not enough coins in your wallet. Missing " + e.missing.getValue + " satoshis (including fees)")
      case ee: Exception =>
        ee.printStackTrace()
        completeWithError(ee.getMessage)
    }
  }

  def receive = {

    case StartWalletKit(seedData) =>
      def receive(wallet: Wallet, tx: Transaction, prevBal: Coin, newBal: Coin): Unit = {
        println("-------------------------------------------> Money received")
        val content =
          ("sessionURI" -> sessionId) ~
          ("address" -> s"${wallet.currentReceiveAddress}") ~
          ("tx" -> tx.getHashAsString) ~
          ("prevBalance" -> prevBal.toString) ~
          ("newBalance" -> newBal.toString)
        println(compact(render(content)))
        SessionManager.cometMessageByHost(sessionId, compact(render(
          ("msgType" -> "balanceChanged") ~ ("content" -> content))))
      }
      def send(wallet: Wallet, tx: Transaction, prevBal: Coin, newBal: Coin): Unit = {
        println("-------------------------------------------> Money sent")
        val content =
            ("sessionURI" -> sessionId) ~
            ("address" -> s"${wallet.currentReceiveAddress}") ~
            ("tx" -> tx.getHashAsString) ~
            ("prevBalance" -> prevBal.toString) ~
            ("newBalance" -> newBal.toString)
        println(compact(render(content)))
        SessionManager.cometMessageByHost(sessionId, compact(render(
          ("msgType" -> "balanceChanged") ~ ("content" -> content))))
      }
      def confidence(wallet: Wallet, tx: Transaction): Unit = {
        println("-------------------------------------------> Confidence changed")
        val content = ("sessionURI" -> sessionId) ~ ("tx" -> tx.getHashAsString) ~ ("confidence" -> tx.getConfidence.getDepthInBlocks)
        println(compact(render(content)))
        SessionManager.cometMessageByHost(sessionId, compact(render(
          ("msgType" -> "confidenceChanged") ~ ("content" -> content))))
      }

      if(!seedData.isEmpty) {
        Try(WalletProvider.restoreWallet(parse(seedData).extract[DeterministicSeedData],
          sessionId, receive, send, confidence)) match {
          case Failure(e) =>
            println(s"----------------------------> Error starting wallet for ${new URI(sessionId).getHost}")
            e.printStackTrace()
          case Success(r) =>
            println(s"----------------------------> Started wallet for ${new URI(sessionId).getHost}")
        }
      } else {
        println(s"----------------------------> Error: no wallet found ${new URI(sessionId).getHost}")
      }

    case SendAmps(address, coin, kit, ctx) =>
      sendCoins(address, coin, kit, ctx)

    case SetSessionTimeout(t) =>
      sessionTimeout = t

    case SetPongTimeout(t) =>
      pongTimeout = t

    case SessionPing(reqCtx) =>
      itemReceived("sessionPing", None)
      resetAliveTimer()
      for ((_, tmr) <- optReq) tmr.cancel()
      msgs match {
        case Nil =>
          optReq = Some(reqCtx, context.system.scheduler.scheduleOnce(pongTimeout, self, PongTimeout))
        case _ =>
          sendMessages(msgs.reverse, reqCtx)
          msgs = Nil
      }

    case PongTimeout =>
      optReq match {
        case Some((req, _)) => {
          req.complete(
            HttpResponse(entity = compact(render(List(("msgType" -> "sessionPong") ~ ("content" -> ("sessionURI" -> sessionId)))))))
          itemSent("sessionPong", None)
          optReq = None
        }
        case None => ()
      }

    case CloseSession(optReqCtx) =>
      context.stop(self)
      SessionManager.removeSession(sessionId)
      for (reqCtx <- optReqCtx) reqCtx.complete(StatusCodes.OK, "session closed")

    case SessionTimedOut =>
      context.self ! CloseSession(None)

    case RunFunction(fn, msgType, content) =>
      itemReceived(msgType, Some(content))
      fn(content)

    case CometMessageList(data) =>
      data.foreach(msg => msgs = msg :: msgs)
      trySendMessages()

    case CometMessage(data) =>
      msgs = data :: msgs
      trySendMessages()

    case StartCamera(reqCtx) =>
      val msg: String = camera match {
        case Some(_) => "session is already recording"
        case None    => "session recording started"
      }
      if (camera.isEmpty) camera = Some(Nil)
      itemReceived("startSessionRecording", None)
      reqCtx.complete(StatusCodes.OK, msg)

    case StopCamera(reqCtx) =>
      itemReceived("stopSessionRecording", None)
      camera match {
        case None =>
          reqCtx.complete(StatusCodes.PreconditionFailed, "session not recording")

        case Some(itms) =>
          val items = itms.reverse.map(itemToString)
          reqCtx.complete(HttpResponse(entity = "[" + items.mkString(",") + "]"))
      }
  }
}

