package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.lift.lib._

import com.typesafe.config._

import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.routing._
import directives.CompletionMagnet
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._
import scala.concurrent.duration._
import java.util.Date
import spray.httpx.encoding._
import org.json4s._
import org.json4s.native.JsonMethods._
import com.biosimilarity.evaluator.msgs._
import java.util.UUID

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class EvaluatorServiceActor extends Actor with EvaluatorService {

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
case class CometMessage(id: String, data: HttpBody, counter: Long = 0)

/**
 * BroadcastMessage used when some data needs to be sent to all
 * alive clients
 * @param data
 */
case class BroadcastMessage(data: HttpBody)

/**
 * Poll used when a client wants to register/long-poll with the
 * server
 */
case class SessionPing(sessionURI: String, reqCtx: RequestContext)

/**
 * PollTimeout sent when a long-poll requests times out
 */
case class PollTimeout(id: String)

/**
 * ClientGc sent to deregister a client when it hasnt responded
 * in a long time
 */
case class ClientGc(id: String)

class CometActor extends Actor {
  var aliveTimers: Map[String, Cancellable] = Map.empty   // list of timers that keep track of alive clients
  var toTimers: Map[String, Cancellable] = Map.empty      // list of timeout timers for clients
  var requests: Map[String, RequestContext] = Map.empty   // list of long-poll RequestContexts

  val gcTime = 1 minute               // if client doesnt respond within this time, its garbage collected
  val clientTimeout = 7 seconds       // long-poll requests are closed after this much time, clients reconnect after this
  val rescheduleDuration = 5 seconds  // reschedule time for alive client which hasnt polled since last message
  val retryCount = 10                 // number of reschedule retries before dropping the message

  def receive = {
    case SessionPing(sessionURI, reqCtx) =>
      requests += (sessionURI -> reqCtx)
      toTimers.get(sessionURI).map(_.cancel())
      toTimers += (sessionURI -> context.system.scheduler.scheduleOnce(clientTimeout, self, PollTimeout(sessionURI)))

      aliveTimers.get(sessionURI).map(_.cancel())
      aliveTimers += (sessionURI -> context.system.scheduler.scheduleOnce(gcTime, self, ClientGc(sessionURI)))

    case PollTimeout(id) =>
      requests.get(id).map(_.complete(HttpResponse(StatusCodes.OK)))
      requests -= id
      toTimers -= id

    case ClientGc(id) =>
      requests -= id
      toTimers -= id
      aliveTimers -= id

    case CometMessage(id, data, counter) =>
      val reqCtx = requests.get(id)
      reqCtx.map { reqCtx =>
        reqCtx.complete(HttpResponse(entity=data))
        requests = requests - id
      } getOrElse {
        if(aliveTimers.contains(id) && counter < retryCount) {
          context.system.scheduler.scheduleOnce(rescheduleDuration, self, CometMessage(id, data, counter+1))
        }
      }

    case BroadcastMessage(data) =>
      aliveTimers.keys.map { id =>
        requests.get(id).map(_.complete(HttpResponse(entity=data))).getOrElse(self ! CometMessage(id, data))
      }
      toTimers.map(_._2.cancel)
      toTimers = Map.empty
      requests = Map.empty
  }
}

case class MalformedRequestException() extends Exception
case class InitializeSessionException(agentURI: String, message: String) extends Exception
case class EvalException(sessionURI: String) extends Exception
case class CloseSessionException(sessionURI: String, message: String) extends Exception

// this trait defines our service behavior independently from the service actor
trait EvaluatorService extends HttpService {
  import DSLCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._
  import DSLCommLinkCtor._

  implicit val formats = DefaultFormats
  val cometActor = actorRefFactory.actorOf(Props[CometActor])
  var _config : Option[Config] = None
    
  def evalConfig() : Config = {
    _config match {
      case Some( cfg ) => cfg
      case None => {
	val cfg =
	  ConfigFactory.load(
	    ConfigFactory.parseFile(
	      new java.io.File( "eval.conf" )
	    )
	  )
	_config = Some( cfg )
	cfg
      }
    }    
  }
  var _link : Option[
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ] = None

  def link() : Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] = {
    _link match {
      case Some( lnk ) => lnk
      case None => {
	val dslCommLinkHost =
	  try {
	    evalConfig.getString( "DSLCommLinkHost" )
	  }
	  catch {
	    case e : Throwable => "10.0.1.10"
	  }
	val dslCommLinkPort = 
	  try {
	    evalConfig.getInt( "DSLCommLinkPort" )
	  }
	  catch {
	    case e : Throwable => 5672
	  }
	val dslCommLinkRemoteHost = 
	  try {
	    evalConfig.getString( "DSLCommLinkRemoteHost" )
	  }
	  catch {
	    case e : Throwable => "10.0.1.8"
	  }
	val dslCommLinkRemotePort = 
	  try {
	    evalConfig.getInt( "DSLCommLinkRemotePort" )
	  }
	  catch {
	    case e : Throwable => 5672
	  }

	val lnk : Being.PersistedMonadicKVDBNode[
	  PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
	] = DSLCommLinkCtor.link()
	_link = Some( lnk )
	lnk
      }
    }    
  }

  val myRoute =
    path("signup") {
      get {
        _.redirect("http://64.27.3.17:6080/agentui.html?uuid=" + UUID.randomUUID(), MovedPermanently)
      }
    } ~
    path("api") {
      post {
        decodeRequest(NoEncoding) {
          // unmarshal with in-scope unmarshaller
          entity(as[String]) { jsonStr =>
            try {
              val json = parse(jsonStr)
              val msgType = (json \ "msgType").extract[String]
              msgType match {
                case "initializeSessionRequest" => {
                  val agentURI = (json \ "content" \ "agentURI").extract[String]
                  val uri = new java.net.URI(agentURI)

                  if (uri.getScheme() != "agent") {
                    throw InitializeSessionException(agentURI, "Unrecognized scheme")
                  }
                  if (uri.getUserInfo() != "George.Costanza:Bosco") {
                    throw InitializeSessionException(agentURI, "Unrecognized user/password combination")
                  }
                  if (uri.getPath() != "/TheAgency") {
                    throw InitializeSessionException(agentURI, "Unrecognized agent ID")
                  }
                
                  complete(HttpResponse(entity = HttpBody(`application/json`,
"""{"msgType": "initializeSessionResponse", "content": {
  "sessionURI": "agent-session://ArtVandelay@session1",
  "listOfAliases": [],
  "listOfLabels": [],
  "lastActiveFilter": ""
}}
"""
                  )))
                }
                case "sessionPing" => {
                  val sessionURI = (json \ "content" \ "sessionURI").extract[String]
                  // TODO: check sessionURI validity
                  (cometActor ! SessionPing(sessionURI, _))
                }
                case "evalRequest" => {
                  val sessionURI = (json \ "content" \ "sessionURI").extract[String]
                  val expression = (json \ "content" \ "expression").extract[String]
                  if (sessionURI != "agent-session://ArtVandelay@session1") {
                    throw EvalException(sessionURI)
                  }
                  cometActor ! CometMessage(sessionURI, HttpBody(`application/json`,
"""{
  "msgType": "evalComplete",
  "content": {
    "sessionURI": "agent-session://ArtVandelay@session1",
    "pageOfPosts": []
  }
}
"""               ))
                  complete(HttpResponse(200, "OK\n"))
                }
                case "closeSessionRequest" => {
                  val sessionURI = (json \ "content" \ "sessionURI").extract[String]
                  if (sessionURI != "agent-session://ArtVandelay@session1") {
                    throw CloseSessionException(sessionURI, "Unknown session.")
                  }
                  cometActor ! CometMessage(sessionURI, HttpBody(`application/json`,
"""{
  "msgType": "closeSessionResponse",
  "content": {
    "sessionURI": "agent-session://ArtVandelay@session1",
  }
}
"""
                  ))
                  complete(HttpResponse(200, "OK\n"))
                }
                case _ => complete(HttpResponse(404, "Unknown message type: " + msgType + "\n"))
              }
            } catch {
              case th: Throwable => complete(HttpResponse(404, "Malformed request"))
            }
          } // jsonStr
        } // decodeRequest
      } // post
    } ~
    path("sendMessage") {
      get {
        parameters('name, 'message) { (name:String, message:String) =>
          cometActor ! BroadcastMessage(HttpBody("%s : %s".format(name, message)))
          complete(StatusCodes.OK)
        }
      }
    } ~
    pathPrefix("static" / PathElement) { dir =>
      getFromResourceDirectory(dir)
    }
}
