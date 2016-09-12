package com.biosimilarity.evaluator.api

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfConfig._
import com.biosimilarity.evaluator.spray.CapUtilities
import com.biosimilarity.evaluator.spray.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.HttpMethods.POST
import spray.http.{HttpRequest, HttpEntity, HttpResponse, ContentType, MediaTypes}

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

trait ApiClient extends CapUtilities {

  implicit val formats = org.json4s.DefaultFormats

  val logger: Logger = LoggerFactory.getLogger(classOf[ApiClient])

  // Set up the Spray client's "Host-level" API for doing requests over TLS
  def eventualHostConnector(sys: ActorSystem)(implicit ec: ExecutionContext, timeout: Timeout): Future[ActorRef] =
    IO(Http)(sys)
      .ask(Http.HostConnectorSetup("localhost", port = serverSSLPort, sslEncryption = true)(sys, clientSSLEngineProvider))
      .mapTo[Http.HostConnectorInfo]
      .map((hci: Http.HostConnectorInfo) => hci.hostConnector)

  type SessionUri = String
  type AgentUri   = String

  def toHttpRequest(cont: RequestContent): HttpRequest =
    HttpRequest(POST, "/api", entity = HttpEntity(ContentType(MediaTypes.`application/json`), write(cont.asRequest)))

  def post(hc: ActorRef, cont: RequestContent)(implicit timeout: Timeout): Future[HttpResponse] =
    hc.ask(toHttpRequest(cont)).mapTo[HttpResponse]

  def getAgentURI(hc: ActorRef, email: String, password: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[AgentUri] =
    post(hc, GetAgentRequest(email, password)).map { (resp: HttpResponse) =>
      val jsv: JValue  = parse(resp.entity.asString)
      val tmsg: String = (jsv \ "msgType").extract[String]
      if (tmsg == "getAgentError") {
        throw new Exception("create user failed, reason : " + (jsv \ "content" \ "reason").extract[String])
      } else {
        (jsv \ "content" \ "agentURI").extract[String]
      }
    }

  def processCreateUserStep1Response(response: HttpResponse, srpClient: SRPClient, email: String, pwd: String)(implicit ec: ExecutionContext) =
    Future {
      parse(response.entity.asString).extract[Response].responseContent match {
        case CreateUserStep1Response(salt) =>
          srpClient.calculateX(email, pwd, salt)
          salt
      }
    }

  def processCreateUserStep2Response(response: HttpResponse)(implicit ec: ExecutionContext) =
    Future {
      parse(response.entity.asString).extract[Response].responseContent match {
        case CreateUserStep2Response(agentURI) => agentURI
      }
    }

  def createSRPUser(hc: ActorRef, email: String, username: String, pwd: String)(implicit ec: ExecutionContext,
                                                                                timeout: Timeout): Future[AgentUri] = {
    val srpClient = new SRPClient()
    srpClient.init
    val blob = JObject(("name", JString(username)) :: Nil)
    val nce  = s"noConfirm:$email"
    for {
      resp1 <- post(hc, CreateUserStep1Request(nce))
      salt  <- processCreateUserStep1Response(resp1, srpClient, email, pwd)
      resp2 <- post(hc, CreateUserStep2Request(nce, salt, srpClient.generateVerifier, blob))
      resp3 <- processCreateUserStep2Response(resp2)
    } yield resp3
  }

  def processInitializeSessionStep2Response(response: HttpResponse, srpClient: SRPClient, email: String, pwd: String)(
    implicit ec: ExecutionContext): Future[String] =
    Future {
      parse(response.entity.asString).extract[Response].responseContent match {
        case InitializeSessionStep1Response(s, b) =>
          srpClient.calculateX(email, pwd, s)
          b
      }
    }

  def processInitializeSessionResponse(response: HttpResponse, srpClient: SRPClient)(implicit ec: ExecutionContext) =
    Future {
      parse(response.entity.asString).extract[Response].responseContent match {
        case InitializeSessionResponse(sessionURI, m2) if srpClient.verifyServerEvidenceMessage(fromHex(m2)) =>
          sessionURI
      }
    }

  def openSRPSession(hc: ActorRef, email: String, pwd: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] = {
    val srpClient = new SRPClient()
    srpClient.init
    for {
      agentUri <- getAgentURI(hc, email, pwd)
      resp1    <- post(hc, InitializeSessionStep1Request("%s?A=%s".format(agentUri, srpClient.calculateAHex)))
      b        <- processInitializeSessionStep2Response(resp1, srpClient, email, pwd)
      resp2    <- post(hc, InitializeSessionStep2Request("%s?M=%s".format(agentUri, srpClient.calculateMHex(b))))
      resp3    <- processInitializeSessionResponse(resp2, srpClient)
    } yield resp3
  }

  def spawnSession(hc: ActorRef, ssn: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] = {
    val cont = SpawnSessionRequest(ssn)
    post(hc, cont).map((response: HttpResponse) => {
      val ent    = response.entity.asString
      val rsp    = parse(ent)
      val newssn = (rsp \ "content" \ "sessionURI").extract[SessionUri]
      newssn
    })
  }

  def startCam(hc: ActorRef, ssn: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] = {
    val cont = StartSessionRecording(ssn)
    post(hc, cont).map((response: HttpResponse) => ssn)
  }

  def stopCam(hc: ActorRef, ssn: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] = {
    val cont = StopSessionRecording(ssn)
    post(hc, cont).map((response: HttpResponse) => response.entity.asString)
  }

  def openAdminSession(hc: ActorRef)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] =
    openSRPSession(hc, readString("nodeAdminEmail"), readString("nodeAdminPass"))

  def capFromAgentUri(agent: AgentUri): String = agent.replace("agent://cap/", "").slice(0, 36)

  def makeAliasUri(agent: AgentUri): String = s"alias://${capFromAgentUri(agent)}/alias"

  def makeConnection(hc: ActorRef, sessionId: SessionUri, agentL: AgentUri, agentR: AgentUri, cnxnLabel: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[Connection] = {
    val sourceUri: String                = makeAliasUri(agentL)
    val targetUri: String                = makeAliasUri(agentR)
    val cont: EstablishConnectionRequest = EstablishConnectionRequest(sessionId, sourceUri, targetUri, cnxnLabel)
    post(hc, cont).map((response: HttpResponse) => Connection(sourceUri, targetUri, cnxnLabel))
  }

  def makePost(hc: ActorRef, sessionId: SessionUri, tgts: List[Connection], lbl: String, uid: String, value: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[Connection] = {
    val from: SessionUri           = "agent://" + capFromSession(sessionId)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: tgts, lbl, value, uid)
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionId, EvalSubscribeExpression("insertContent", cont))
    post(hc, req).map((response: HttpResponse) => selfcnxn)
  }

  def makeQueryOnSelf(hc: ActorRef, sessionId: SessionUri, lbl: String)(implicit ec: ExecutionContext,
                                                                        timeout: Timeout): Future[Connection] = {
    val from: SessionUri           = "agent://" + capFromSession(sessionId)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: Nil, lbl, "", "")
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionId, EvalSubscribeExpression("feedExpr", cont))
    post(hc, req).map((response: HttpResponse) => selfcnxn)
  }

  def getConnectionProfiles(hc: ActorRef, sessionId: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] = {
    val cont: GetConnectionProfiles = GetConnectionProfiles(sessionId)
    post(hc, cont).map((response: HttpResponse) => response.entity.asString)
  }

  def sessionPing(hc: ActorRef, ssn: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] = {
    val cont: SessionPing = SessionPing(ssn)
    post(hc, cont).map((response: HttpResponse) => parse(response.entity.asString).extract[JArray])
  }

  def pingUntilPong(hc: ActorRef, ssn: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] = {
    def _step(acc: List[JValue]): List[JValue] = {
      val ja = Await.result(sessionPing(hc, ssn), FiniteDuration(10, SECONDS))
      var ta = acc

      var done = false
      ja.arr.foreach((jv: JValue) => {
        val msgType = (jv \ "msgType").extract[String]
        if (msgType == "sessionPong") done = true
        else ta = jv :: ta
      })

      if (done) ta
      else _step(ta)
    }
    val l = _step(Nil)
    Future.successful(JArray(l))
  }
}
