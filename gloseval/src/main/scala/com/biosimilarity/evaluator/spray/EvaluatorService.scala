package com.biosimilarity.evaluator.spray

import akka.actor._
import com.biosimilarity.lift.lib._
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.spray.util.HttpsDirectives
import spray.routing._

import java.util.UUID

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import spray.http._
import spray.httpx.encoding._

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class CometMessage(data: String)
case class CometMessageList(data: List[String])
case class SessionPing(reqCtx: RequestContext) extends Serializable
case class PongTimeout() extends Serializable
case class SetSessionId(id : String) extends Serializable
case class KeepAlive() extends Serializable
case class RunFunction(fn : JObject => Unit, msgType : String, content : JObject) extends Serializable
case class CloseSession(reqCtx : Option[RequestContext]) extends Serializable
case class CameraItem(sending : Boolean, msgType : String, content : Option[JObject], tmStamp : DateTime)
case class StartCamera(reqCtx : RequestContext)
case class StopCamera(reqCtx : RequestContext)
case class SetPongTimeout(t : FiniteDuration)
case class SetSessionTimeout(t : FiniteDuration)

object ChunkingActor {
  case class SetChunkSize(sz : Int)
  case class SetExpected(n : Int)
  case class SetRecipient(ref : ActorRef)
}

class ChunkingActor extends Actor {
  import ChunkingActor._

  var chunkSize = 20
  var expected = -1
  var recipient : Option[ActorRef] = None

  var msgs : List[String] = Nil
  def receive = {
    case SetRecipient(ref) => recipient = Some(ref)

    case SetChunkSize(sz) => chunkSize = sz

    case SetExpected(sz) => expected = sz

    case CometMessage(data) =>
      msgs = data :: msgs
      expected -= 1
      if (msgs.length == chunkSize || expected == 0) {
        recipient match {
          case Some(ref) =>
            ref ! CometMessageList(msgs.reverse)
            msgs = Nil
            if (expected == 0) context.stop(self)

          case None =>
            throw new Exception("Recipient not set for chunking actor")
        }
      }
  }
}

class SessionActor extends Actor with Serializable {

  // if client doesnt receive any messages within this time, its garbage collected
  var sessionTimeout = EvalConfConfig.readInt("sessionTimeoutMinutes") minutes

  // ping requests are ponged after this much time unless other data is sent in the meantime
  // clients need to re-ping after this
  var pongTimeout = EvalConfConfig.readInt("pongTimeoutSeconds") seconds

  @transient
  var aliveTimer = context.system.scheduler.scheduleOnce(pongTimeout, self, CloseSession(None))
  @transient
  var optReq : Option[(RequestContext, Cancellable)] = None
  @transient
  var msgs : List[String] = Nil
  @transient
  var camera : Option[List[CameraItem]] = None
  var sessionId : Option[String] = None
  @transient
  implicit val formats = DefaultFormats

  override def postStop() = {
    sessionId = None
  }

  def resetAliveTimer() : Unit = {
    aliveTimer.cancel()
    aliveTimer = context.system.scheduler.scheduleOnce(sessionTimeout, self, CloseSession(None))
  }

  def itemToString(itm : CameraItem) : String = {
    val cont : JValue = itm.content match {
      case Some(j) => j
      case None => JNull

    }
    val jo : JObject =
      ("msgType" -> itm.msgType) ~
      ("direction" -> (if (itm.sending) "Sent" else "Received") ) ~
      ("tmStamp" -> itm.tmStamp.toIsoDateTimeString) ~
      ("content" -> cont)
    pretty(render(jo))
  }

  def addCameraItem(itm : CameraItem) : Unit = {
    camera match {
      case Some(l) => camera = Some(itm :: l)
      case None => ()
    }
  }

  def itemReceived(msgType : String, content : Option[JObject]) : Unit = {
    if (camera.isDefined) addCameraItem(CameraItem(sending = false, msgType, content, DateTime.now))
  }

  def itemSent(msgType : String, content : Option[JObject]) : Unit = {
    if (camera.isDefined) addCameraItem(CameraItem(sending = true, msgType, content, DateTime.now))
  }

  def sendMessages(msgs : List[String], reqCtx : RequestContext) : Unit = {
    val now = DateTime.now
    if (camera.isDefined) {
      msgs.foreach(msg => {
        implicit val formats = DefaultFormats

        val jo = parse(msg)
        val msgType = (jo \ "msgType").extract[String]
        val content = (jo \ "content").extract[JObject]
        addCameraItem(CameraItem(sending = true, msgType, Some(content), now))
      })
    }
    reqCtx.complete(HttpResponse(entity = "[" + msgs.mkString(",") + "]"))
  }

  def trySendMessages() : Unit = {
    resetAliveTimer()
    optReq match {
      case None => () //println("CometMessage optReqCtx miss: id = " + sessionId)

      case Some((reqCtx,tmr)) => {
        tmr.cancel()
        sendMessages(msgs.reverse, reqCtx)
        optReq = None
        msgs = Nil
      }
    }
  }

  def receive = {
    case SetSessionId(id) =>
      sessionId = Some(id)
      resetAliveTimer()

    case SetSessionTimeout(t) =>
      sessionTimeout = t
      resetAliveTimer()

    case SetPongTimeout(t) =>
      pongTimeout = t
      resetAliveTimer()

    case KeepAlive() =>
      resetAliveTimer()

    case SessionPing(reqCtx) =>
      itemReceived("sessionPing", None)
      for (ssn <- sessionId) {
        resetAliveTimer()
        for ((_, tmr) <- optReq) tmr.cancel()
        msgs match {
          case Nil =>
            optReq = Some(reqCtx, context.system.scheduler.scheduleOnce(pongTimeout, self, PongTimeout()))
          case _ =>
            sendMessages( msgs.reverse, reqCtx)
            msgs = Nil
        }
      }

    case PongTimeout() =>
      for (ssn <- sessionId) {
        optReq match {
          case Some((req, _)) => {
            req.complete(HttpResponse(entity = compact(render(
              List(("msgType" -> "sessionPong") ~ ("content" -> ("sessionURI" -> ssn)))))))
            itemSent("sessionPong", None)
            optReq = None
          }
          case None => ()
        }
      }

    case CloseSession(optReqCtx) =>
      context.stop(self)
      for (ssn <- sessionId) SessionManager.removeSession(ssn)
      for (reqCtx <- optReqCtx) reqCtx.complete(StatusCodes.OK,"session closed")

    case RunFunction(fn,msgType,content) =>
      itemReceived(msgType,Some(content))
      fn(content)

    case CometMessageList(data) =>
      data.foreach( msg => msgs = msg :: msgs)
      trySendMessages()

    case CometMessage(data) =>
      msgs = data :: msgs
      trySendMessages()

    case StartCamera(reqCtx) =>
      val msg : String = camera match {
        case Some(_) => "session is already recording"
        case None => "session recording started"
      }
      if (camera.isEmpty) camera = Some(Nil)
      itemReceived("startSessionRecording", None)
      reqCtx.complete(StatusCodes.OK,msg)

    case StopCamera(reqCtx) =>
      itemReceived("stopSessionRecording", None)
      camera match {
        case None =>
          reqCtx.complete(StatusCodes.PreconditionFailed,"session not recording" )

        case Some(itms) =>
          val items = itms.reverse.map(itemToString)
          reqCtx.complete(HttpResponse(entity = "[" + items.mkString(",") + "]"))
      }
  }
}

object EvaluatorServiceActor {
  case class initSession(actor: ActorRef, ssn: String)
  case class SetDefaultSessionTimeout(t : FiniteDuration)
  case class SetDefaultPongTimeout(t : FiniteDuration)
}

class EvaluatorServiceActor extends Actor
  with EvaluatorService
  with Serializable {
  import EvalHandlerService._
  import EvaluatorServiceActor._

  createNodeUser(NodeUser.email, NodeUser.password, NodeUser.jsonBlob)

  // if client doesnt receive any messages within this time, its garbage collected
  var defaultSessionTimeout = EvalConfConfig.readInt("sessionTimeoutMinutes") minutes

  // ping requests are ponged after this much time unless other data is sent in the meantime
  // clients need to re-ping after this
  var defaultPongTimeout = EvalConfConfig.readInt("pongTimeoutSeconds") seconds


  def handle: Receive = {
    case SetDefaultSessionTimeout(t) =>
      defaultSessionTimeout = t

    case SetDefaultPongTimeout(t) =>
      defaultPongTimeout = t

    case initSession(actor: ActorRef, ssn: String) =>
      actor ! SetSessionTimeout(defaultSessionTimeout)
      actor ! SetPongTimeout(defaultPongTimeout)
      actor ! SetSessionId(ssn)


  }

  def receive = handle orElse runRoute(myRoute)

  def actorRefFactory = context

  SessionManager.setSessionManager(context)

}

case class InitializeSessionException(agentURI: String, message: String) extends Exception with Serializable

trait EvaluatorService extends HttpService with HttpsDirectives with CORSSupport {

  import EvalHandlerService._

  @transient
  val syncMethods = HashMap[String, (JObject, String) => Unit](
    // Old stuff
    ("createUserRequest", createUserRequest),
    ("confirmEmailToken", confirmEmailToken),
    // New API
    ("createAgentRequest", createAgentRequest),
    ("initializeSessionRequest", initializeSessionRequest),
    ("getAgentRequest", getAgentRequest),
    ("spawnSessionRequest", spawnSessionRequest),
    ("createUserStep1Request", createUserStep1Request),
    ("createUserStep2Request", createUserStep2Request),
    ("initializeSessionStep1Request", initializeSessionStep1Request),
    ("initializeSessionStep2Request", initializeSessionStep2Request)
  )

  @transient
  val asyncMethods = HashMap[String, JObject => Unit](
    // Old stuff
    ("updateUserRequest", updateUserRequest),
    // Agents
    ("addAgentExternalIdentityRequest", addAgentExternalIdentityRequest),
    ("addAgentExternalIdentityToken", addAgentExternalIdentityToken),
    ("removeAgentExternalIdentitiesRequest", removeAgentExternalIdentitiesRequest),
    ("getAgentExternalIdentitiesRequest", getAgentExternalIdentitiesRequest),
    ("addAgentAliasesRequest", addAgentAliasesRequest),
    ("removeAgentAliasesRequest", removeAgentAliasesRequest),
    ("getAgentAliasesRequest", getAgentAliasesRequest),
    ("getDefaultAliasRequest", getDefaultAliasRequest),
    ("setDefaultAliasRequest", setDefaultAliasRequest),
    // Aliases
    ("addAliasExternalIdentitiesRequest", addAliasExternalIdentitiesRequest),
    ("removeAliasExternalIdentitiesRequest", removeAliasExternalIdentitiesRequest),
    ("getAliasExternalIdentitiesRequest", getAliasExternalIdentitiesRequest),
    ("setAliasDefaultExternalIdentityRequest", setAliasDefaultExternalIdentityRequest),
    // Connections
    ("removeAliasConnectionsRequest", removeAliasConnectionsRequest),
    ("getAliasConnectionsRequest", getAliasConnectionsRequest),
    ("getConnectionProfiles", getConnectionProfiles),
    // Alias Labels
    ("addAliasLabelsRequest", addAliasLabelsRequest),
    ("updateAliasLabelsRequest", updateAliasLabelsRequest),
    ("getAliasLabelsRequest", getAliasLabelsRequest),
    ("setAliasDefaultLabelRequest", setAliasDefaultLabelRequest),
    ("getAliasDefaultLabelRequest", getAliasDefaultLabelRequest),
    // System Labels
    //("addSystemLabelsRequest", addSystemLabelsRequest),
    // DSL
    ("evalSubscribeRequest", evalSubscribeRequest),
    ("evalSubscribeCancelRequest", evalSubscribeCancelRequest),
    // Introduction Protocol
    ("beginIntroductionRequest", beginIntroductionRequest),
    ("introductionConfirmationRequest", introductionConfirmationRequest),
    ("establishConnectionRequest", establishConnectionRequest),
    // Database dump/restore
    ("backupRequest", backupRequest),
    ("restoreRequest", restoreRequest),
    // Verifier protocol
    ("initiateClaim", initiateClaim),
    // omni
    ("omniGetBalance", omniGetBalance),
    ("omniTransfer", omniTransfer),
    ("getAmpWalletAddress", omniGetAmpWalletAddress),
    ("setAmpWalletAddress", omniSetAmpWalletAddress)
    )

  @transient
  val myRoute =
    requireHttps {
      cors {
        path("api") {
          post {
            decodeRequest(NoEncoding) {
              entity(as[String]) { jsonStr =>
                (ctx) => {
                  try {
                    //BasicLogService.tweet("json: " + jsonStr)
                    val json = parse(jsonStr)
                    val msgType = (json \ "msgType").extract[String]
                    val content = (json \ "content").extract[JObject]
                    syncMethods.get(msgType) match {
                      case Some(fn) => {
                        val key = UUID.randomUUID.toString
                        CompletionMapper.map += (key -> ctx)
                        fn(content, key)
                      }
                      case None => (content \ "sessionURI").extractOpt[String] match {
                        case None => {
                          ctx.complete(StatusCodes.Forbidden, "missing sessionURI parameter")
                        }
                        case Some(sessionURI) => {
                          SessionManager.getSession(sessionURI) match {
                            case None => {
                              ctx.complete(StatusCodes.Forbidden,"Invalid sessionURI parameter")
                            }
                            case Some(cometActor) => msgType match {
                              case "sessionPing" => {
                                cometActor ! SessionPing(ctx)
                              }
                              case "startSessionRecording" => cometActor ! StartCamera(ctx)
                              case "stopSessionRecording" => cometActor ! StopCamera(ctx)
                              case "closeSessionRequest" => cometActor ! CloseSession(Some(ctx))
                              case _ => {
                                cometActor ! KeepAlive()
                                asyncMethods.get(msgType) match {
                                  case Some(fn) => {
                                    cometActor ! RunFunction(fn, msgType, content)
                                    ctx.complete(StatusCodes.OK)
                                  }
                                  case _ => {
                                    ctx.complete(HttpResponse(500, "Unknown message type: " + msgType + "\n"))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  catch {
                    case th: Throwable => {
                      val writer: java.io.StringWriter = new java.io.StringWriter()
                      val printWriter: java.io.PrintWriter = new java.io.PrintWriter(writer)
                      th.printStackTrace(printWriter)
                      printWriter.flush()

                      val stackTrace: String = writer.toString()
                      //println( "Malformed request: \n" + stackTrace )
                      BasicLogService.tweet("Malformed request: \n" + stackTrace)
                      ctx.complete(HttpResponse(500, "Malformed request: \n" + stackTrace))
                    }
                  }
                }
              } // jsonStr
            } // decodeRequest
          } // post
        } ~
          pathPrefix("agentui") {
            getFromDirectory("./agentui")
          }
      }
    }
}

/*
        path("admin/connectServers") {
          // allow administrators to make
          // sure servers are connected
          // BUGBUG : lgm -- make this secure!!!
          get {
            parameters('whoAmI) {
              (whoAmI: String) =>
                {
                  //             println(
                  //               (
                  //                 " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "
                  //                 + "in admin/connectServers2 "
                  //                 + " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "
                  //               )
                  //             )
                  BasicLogService.tweet(
                    (
                      " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "
                      + "in admin/connectServers2 "
                      + " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "))

                  connectServers("evaluator-service", UUID.randomUUID)

                  (cometActor ! SessionPing("", _))
                }
            }
          }
        } ~
        pathPrefix("splicious/btc") {
          get {
            parameters('btcToken) {
              (btcToken: String) =>
                {
                  BasicLogService.tweet(
                    (
                      " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "
                      + "in splicious/btc "
                      + " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "))

                  handleBTCResponse(btcToken)

                  (cometActor ! SessionPing("", _))

                }
            }
          }
        }
  */
