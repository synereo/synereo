package com.biosimilarity.evaluator.spray

import akka.actor._
import com.biosimilarity.lift.lib._
import spray.routing._

import scala.collection.mutable
import java.util.UUID

import com.biosimilarity.evaluator.omniRPC.{OmniClient, OmniConfig}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import spray.http._
import spray.httpx.encoding._

import scala.collection.mutable.{HashMap, HashSet}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class EvaluatorServiceActor extends Actor
  with EvaluatorService
  with Serializable {
  import EvalHandlerService._

  // create well-known node user
  createNodeUser(NodeUser.email, NodeUser.password, NodeUser.jsonBlob)

  if (OmniConfig.read(OmniClient.OMNI_URI,"miss") != "miss" && !OmniClient.canConnect()) throw new Exception("Unable to connect to OmniCore")

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}

/**
 * CometMessage used when some data needs to be sent to a client
 * with the given id
 */
case class CometMessage(id: String, data: String)
  extends Serializable

/**
 * Poll used when a client wants to register/long-poll with the
 * server
 */
case class SessionPing(sessionURI: String, reqCtx: RequestContext) extends Serializable

/**
 * PollTimeout sent when a long-poll requests times out
 */
case class PollTimeout(id: String) extends Serializable

/**
 * ClientGc sent to deregister a client when it hasnt pinged in a long time
 */
case class ClientGc(id: String) extends Serializable

class CometActor extends Actor with Serializable {
  @transient
  val aliveTimers = new mutable.HashMap[String, Cancellable] // list of timers that keep track of alive clients
  @transient
  val toTimers = new mutable.HashMap[String, Cancellable] // list of timeout timers for clients
  @transient
  val requests = new mutable.HashMap[String, RequestContext] // list of long-poll RequestContexts
  @transient
  val sets = new mutable.HashMap[String, HashSet[String]] // sets of async return messages

  val gcTime = 3 minutes // if client doesnt poll within this time, its garbage collected
  val clientTimeout = 7 seconds // poll requests are closed after this much time, clients repoll (ping) after this


  //@@GS - testing theory: all methods of cometActor are synchronized => cometMapLock is redundant
  //@transient
  //lazy val cometMapLock = new scala.concurrent.Lock()

  def receive = {
    case SessionPing(id, reqCtx) => synchronized {
      //cometMapLock.acquire()
      aliveTimers.get(id).map(_.cancel())
      aliveTimers += (id -> context.system.scheduler.scheduleOnce(gcTime, self, ClientGc(id)))

      def _wait() {
        requests += (id -> reqCtx)
        toTimers.get(id).map(_.cancel())
        toTimers += (id -> context.system.scheduler.scheduleOnce(clientTimeout, self, PollTimeout(id)))
      }

      sets.get(id) match {
        case None => {
          _wait()
        }
        case Some(set) => {
          // If there are messages, forward them immediately.
          if (set.size == 0) {
            _wait()
          } else {
            sets -= id
            reqCtx.complete(HttpResponse(entity = "[" + set.toList.mkString(",") + "]"))
          }
        }
      }
      //cometMapLock.release()
    }

    case PollTimeout(id) => synchronized {
      //cometMapLock.acquire()
      //println( "In PollTimeout checking for a req to match id = " + id ) 
      for (req <- requests.get(id)) {
        //println( "In PollTimeout about to call complete with sessionPong; req = " + req ) 
        req.complete(HttpResponse(entity = compact(render(
          List(("msgType" -> "sessionPong") ~ ("content" -> ("sessionURI" -> id)))))))
      }
      requests -= id
      toTimers -= id
      //cometMapLock.release()
    }

    case ClientGc(id) => synchronized {
      //cometMapLock.acquire()
      requests -= id
      toTimers -= id
      aliveTimers -= id
      //cometMapLock.release()
    }

    case CometMessage(id, data) => synchronized {
      //cometMapLock.acquire()
      def dataPrintRep() =
        if (data.toString.length > 140) {
          data.toString.substring(0, Math.min(140, data.toString.length)) + "...}"
        } else {
          data.toString
        }

      val set = sets.getOrElse(id, {
        println(
          (
            "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
              + "\nCometMessage id miss: id = " + id
              + ", data = " + dataPrintRep()
              + "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        val newSet = new mutable.HashSet[String]
        sets += (id -> newSet)
        newSet
      })
      set += data

      def setsPrintRep() =
        if (sets.toString.length > 140) {
          sets.toString.substring(0, Math.min(140, sets.toString.length)) + "...}"
        } else {
          sets.toString
        }
      val optReqCtx = requests.get(id)
      BasicLogService.tweet("CometMessage: id = " + id + ", data = " + data + ", sets = " + sets + ", optReqCtx = " + optReqCtx)
      //println("CometMessage: id = " + id + ", data = " + data + ", sets = " + sets + ", optReqCtx = " + optReqCtx)
      optReqCtx match {
        case None => {
          println(
            (
              "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
              + "\nCometMessage optReqCtx miss: id = " + id
              + ", data = " + dataPrintRep()
              + ", sets = " + setsPrintRep()
              + "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        }
        case Some(reqCtx) => {
          requests -= id
          sets -= id
          //println("calling reqCtx.complete on " + reqCtx)
          reqCtx.complete(HttpResponse(entity = "[" + set.toList.mkString(",") + "]"))
        }
      }
      //cometMapLock.release()
    }
  }
}

case class MalformedRequestException() extends Exception with Serializable
case class InitializeSessionException(agentURI: String, message: String) extends Exception with Serializable
case class SessionException(message: String) extends Exception with Serializable
case class EvalException(sessionURI: String) extends Exception with Serializable
case class CloseSessionException(sessionURI: String, message: String) extends Exception with Serializable

// this trait defines our service behavior independently from the service actor
trait EvaluatorService extends HttpService with CORSSupport {
  import EvalHandlerService._

  @transient
  val cometActor = actorRefFactory.actorOf(Props[CometActor])

  CometActorMapper.map += (CometActorMapper.key -> cometActor)

  @transient
  val syncMethods = HashMap[String, (JObject, String) => Unit](
    // Old stuff
    ("createUserRequest", createUserRequest),
    ("confirmEmailToken", confirmEmailToken),
    // New API
    ("createAgentRequest", createAgentRequest),
    ("initializeSessionRequest", initializeSessionRequest),
    // gary goofing about
      ("getAgentRequest", getAgentRequest)
  )

  @transient
  val asyncMethods = HashMap[String, JObject => Unit](
    // Old stuff
    ("closeSessionRequest", closeSessionRequest),
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
    ("resetDatabaseRequest", resetDatabaseRequest),
    // Verifier protocol
    ("initiateClaim", initiateClaim),
    // omni interfact
    ("omniGetBalance", omniGetBalance),
    ("omniTransfer", omniTransfer),
    ("getAmpWalletAddress", omniGetAmpWalletAddress),
    ("setAmpWalletAddress", omniSetAmpWalletAddress)

    )

  @transient
  val myRoute =
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
                  asyncMethods.get(msgType) match {
                    case Some(fn) => {
                      /*
                      (content \ "sessionURI").extract[Option[String]] match {
                        case Some(sessionURI) => {
                          //if (!actor.checkSessionAlive(sessionURI)) throw new SessionException("Session no longer valid")
                        }
                        case None => throw new SessionException("Async message missing session info")
                      }
                      */
                      fn(content)
                      ctx.complete(StatusCodes.OK)
                    }
                    case None => syncMethods.get(msgType) match {
                      case Some(fn) => {
                        val key = UUID.randomUUID.toString
                        CompletionMapper.map += (key -> ctx)
                        fn(content, key)
                      }
                      case None => msgType match {
                        case "sessionPing" => {
                          val sessionURI = (content \ "sessionURI").extract[String]
                          (cometActor ! SessionPing(sessionURI, ctx))
                        }
                        case _ => {
                          ctx.complete(HttpResponse(500, "Unknown message type: " + msgType + "\n"))
                        }
                      }
                    }
                  }
                } catch {
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
      path("""admin/connectServers""") {
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
      pathPrefix("agentui") {
        getFromDirectory("./agentui")
      }

    }
}

/*
<<<<<<< HEAD
=======
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
>>>>>>> origin/1.0
       pathPrefix("static" / Segment) { path =>
         getFromFile(path)
       } ~
        pathPrefix("agentui") {
          getFromDirectory("./agentui")
        } ~
        pathPrefix("splicious") {
          getFromDirectory("./agentui")
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
