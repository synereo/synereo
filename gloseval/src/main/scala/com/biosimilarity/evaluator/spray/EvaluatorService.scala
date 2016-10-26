package com.biosimilarity.evaluator.spray

import java.util.UUID

import akka.actor._
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.spray.directives.{CORSSupport, HttpsDirectives}
import com.biosimilarity.lift.lib._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import spray.http._
import spray.httpx.encoding._
import spray.routing._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait EvaluatorService extends HttpService with HttpsDirectives with CORSSupport {

  import EvalHandlerService._

  @transient
  val syncMethods = mutable.HashMap[String, (JObject, String) => Unit](
    // Old stuff
    //("createUserRequest", createUserRequest),

    ("confirmEmailToken", confirmEmailToken),
    ("createAgentRequest", createAgentRequest),
    ("initializeSessionRequest", initializeSessionRequest),
    ("getAgentRequest", getAgentRequest),
    ("spawnSessionRequest", spawnSessionRequest),
    ("createUserStep1Request", createUserStep1Request),
    ("createUserStep2Request", createUserStep2Request),
    ("initializeSessionStep1Request", initializeSessionStep1Request),
    ("initializeSessionStep2Request", initializeSessionStep2Request),
    // Version Info
    ("versionInfoRequest", versionInfoRequest)
  )

  @transient
  val asyncMethods = mutable.HashMap[String, JObject => Unit](
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

  def handleAsyncPosts(ctx: RequestContext, msgType: String, content: JObject): Unit =
    (content \ "sessionURI").extractOpt[String] match {
      case None =>
        ctx.complete(StatusCodes.Forbidden, "Missing sessionURI parameter")
      case Some(sessionURI) =>
        @transient
        val session: Option[ActorRef] = SessionManager.getSession(sessionURI)
        session match {
          case None =>
            ctx.complete(StatusCodes.Forbidden, "Invalid sessionURI parameter")
          case Some(cometActor) =>
            msgType match {
              case "sessionPing"           => cometActor ! SessionActor.SessionPing(ctx)
              case "closeSessionRequest"   => cometActor ! SessionActor.CloseSession(Some(ctx))
              case "startSessionRecording" => cometActor ! SessionActor.StartCamera(ctx)
              case "stopSessionRecording"  => cometActor ! SessionActor.StopCamera(ctx)
              case _ =>
                asyncMethods.get(msgType) match {
                  case Some(fn) =>
                    cometActor ! SessionActor.ItemReceived(msgType, content)
                    ctx.complete(StatusCodes.OK)
                    fn(content)
                  case _ =>
                    ctx.complete(HttpResponse(500, "Unknown message type: " + msgType + "\n"))
                }
            }
        }
    }

  def handlePosts(entityString: String): (RequestContext) => Unit =
    (ctx: RequestContext) =>
      try {
        val json: JValue     = parse(entityString)
        val msgType: String  = (json \ "msgType").extract[String]
        val content: JObject = (json \ "content").extract[JObject]
        syncMethods.get(msgType) match {
          case Some(fn) =>
            val key: String = UUID.randomUUID.toString
            CompletionMapper.hmap += (key -> ctx)
            fn(content, key)
          case None =>
            handleAsyncPosts(ctx, msgType, content)
        }
      } catch {
        case e: Throwable =>
          val writer: java.io.StringWriter     = new java.io.StringWriter()
          val printWriter: java.io.PrintWriter = new java.io.PrintWriter(writer)
          e.printStackTrace(printWriter)
          printWriter.flush()
          val stackTrace: String     = writer.toString()
          val responseString: String = s"""|Malformed request:
                                           |$stackTrace""".stripMargin
          BasicLogService.tweet(responseString)
          ctx.complete(HttpResponse(500, responseString))
    }

  @transient
  val myRoute =
    requireHttps {
      cors {
        path("api") {
          post {
            decodeRequest(NoEncoding) {
              entity(as[String]) { (entityString: String) =>
                handlePosts(entityString)
              }
            }
          }
        } ~ pathPrefix("agentui") {
          getFromDirectory("agentui")
        } ~ (pathSingleSlash | path("index.html")) {
          getFromFile("client/index.html")
        } ~ pathPrefix("assets") {
          getFromDirectory("client/assets")
        } ~ path("logging") {
          post {
            entity(as[String]) { (entityString: String) =>
              BasicLogService.tweet(s"UI: $entityString")
              complete("logged")
            }
          }
        }
      }
    }
}

class EvaluatorServiceActor extends Actor with EvaluatorService with Serializable {

  import EvalHandlerService._

  createNodeUser(NodeUser.email, NodeUser.password, NodeUser.jsonBlob)

  def receive = runRoute(myRoute)

  def actorRefFactory = context

  context.actorOf(Props[SessionManagerActor])
}
