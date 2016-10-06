package com.biosimilarity.evaluator.spray.client

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper._
import com.biosimilarity.evaluator.spray.CapUtilities
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.HttpMethods.POST
import spray.http._
import spray.io.ClientSSLEngineProvider

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

trait ApiClient extends CapUtilities {

  implicit val formats = org.json4s.DefaultFormats

  // Set up the Spray client's "Host-level" API for doing requests over TLS
  def eventualHostConnector(system: ActorSystem, port: Int, sslProvider: ClientSSLEngineProvider)(implicit ec: ExecutionContext,
                                                                                                  timeout: Timeout): Future[ActorRef] =
    IO(Http)(system)
      .ask(Http.HostConnectorSetup("localhost", port, sslEncryption = true)(system, sslProvider))
      .mapTo[Http.HostConnectorInfo]
      .map((hci: Http.HostConnectorInfo) => hci.hostConnector)

  type SessionUri = String
  type AgentUri   = String

  def toHttpRequest(uri: Uri, requestContent: RequestContent): HttpRequest =
    HttpRequest(POST, uri, entity = HttpEntity(ContentType(MediaTypes.`application/json`), write(requestContent.asRequest)))

  def post(hc: ActorRef, uri: Uri, requestContent: RequestContent)(implicit timeout: Timeout): Future[HttpResponse] =
    hc.ask(toHttpRequest(uri, requestContent)).mapTo[HttpResponse]

  def parseHttpResponseEntity(response: HttpResponse): JValue =
    parse(response.entity.asString)

  def extractResponseContentFromJValue(jValue: JValue): ResponseContent =
    jValue.extract[Response].extractResponseContent

  def extractResponseContentFromHttpResponse: (HttpResponse) => ResponseContent =
    parseHttpResponseEntity _ andThen extractResponseContentFromJValue

  def getAgentURI(hc: ActorRef, uri: Uri, email: String, password: String)(implicit ec: ExecutionContext,
                                                                           timeout: Timeout): Future[AgentUri] =
    post(hc, uri, GetAgentRequest(email, password)).map { (response: HttpResponse) =>
      parseHttpResponseEntity(response)
    }.map { (value: JValue) =>
      (value \ "msgType").extract[String] match {
        case "getAgentError" =>
          throw new Exception("create user failed, reason : " + (value \ "content" \ "reason").extract[String])
        case _ =>
          (value \ "content" \ "agentURI").extract[String]
      }
    }

  def processCreateUserStep1Response(response: HttpResponse, srpClient: SRPClient, email: String, password: String)(
      implicit ec: ExecutionContext) =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case CreateUserStep1Response(salt) =>
          srpClient.calculateX(email, password, salt)
          salt
        case _ =>
          throw new Exception(s"Unexpected result when extracting $response")
      }
    }

  def processCreateUserStep2Response(response: HttpResponse)(implicit ec: ExecutionContext) =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case CreateUserStep2Response(agentURI) =>
          agentURI
        case _ =>
          throw new Exception(s"Unexpected result when extracting $response")
      }
    }

  def eventualSRPClient()(implicit ec: ExecutionContext): Future[SRPClient] =
    Future {
      val s: SRPClient = new SRPClient()
      s.init
      s
    }

  def createSRPUser(hc: ActorRef, email: String, username: String, password: String)(implicit ec: ExecutionContext,
                                                                                     timeout: Timeout): Future[AgentUri] =
    for {
      srpClient <- eventualSRPClient()
      uri       <- Future("/api")
      blob      <- Future(JObject(("name", JString(username)) :: Nil))
      nce       <- Future(s"noConfirm:$email")
      resp1     <- post(hc, uri, CreateUserStep1Request(nce))
      salt      <- processCreateUserStep1Response(resp1, srpClient, email, password)
      resp2     <- post(hc, uri, CreateUserStep2Request(nce, salt, srpClient.generateVerifier, blob))
      resp3     <- processCreateUserStep2Response(resp2)
    } yield resp3

  def processInitializeSessionStep2Response(response: HttpResponse, srpClient: SRPClient, email: String, password: String)(
      implicit ec: ExecutionContext): Future[String] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case InitializeSessionStep1Response(s, b) =>
          srpClient.calculateX(email, password, s)
          b
        case _ =>
          throw new Exception(s"Unexpected result when extracting $response")
      }
    }

  def processInitializeSessionResponse(response: HttpResponse, srpClient: SRPClient)(
      implicit ec: ExecutionContext): Future[InitializeSessionResponse] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case isr @ InitializeSessionResponse(_, _, _, _, _, _, _, m2) if srpClient.verifyServerEvidenceMessage(fromHex(m2)) =>
          isr
        case _ =>
          throw new Exception(s"Unexpected result when extracting $response")
      }
    }

  def openSRPSession(hc: ActorRef, uri: Uri, email: String, password: String)(implicit ec: ExecutionContext,
                                                                              timeout: Timeout): Future[InitializeSessionResponse] =
    for {
      srpClient <- eventualSRPClient()
      agentUri  <- getAgentURI(hc, uri, email, password)
      resp1     <- post(hc, uri, InitializeSessionStep1Request("%s?A=%s".format(agentUri, srpClient.calculateAHex)))
      b         <- processInitializeSessionStep2Response(resp1, srpClient, email, password)
      resp2     <- post(hc, uri, InitializeSessionStep2Request("%s?M=%s".format(agentUri, srpClient.calculateMHex(b))))
      resp3     <- processInitializeSessionResponse(resp2, srpClient)
    } yield resp3

  def spawnSession(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] =
    post(hc, uri, SpawnSessionRequest(sessionUri)).map { (response: HttpResponse) =>
      (parseHttpResponseEntity(response) \ "content" \ "sessionURI").extract[SessionUri]
    }

  def startCam(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[SessionUri] =
    post(hc, uri, StartSessionRecording(sessionUri)).map((response: HttpResponse) => sessionUri)

  def stopCam(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    post(hc, uri, StopSessionRecording(sessionUri)).map((response: HttpResponse) => response.entity.asString)

  def openAdminSession(hc: ActorRef, uri: Uri, email: String, password: String)(implicit ec: ExecutionContext,
                                                                                timeout: Timeout): Future[InitializeSessionResponse] =
    openSRPSession(hc, uri, email, password)

  def capFromAgentUri(agent: AgentUri): String =
    agent.replace("agent://cap/", "").slice(0, 36)

  def makeAliasUri(agent: AgentUri): String =
    s"alias://${capFromAgentUri(agent)}/alias"

  def makeConnection(hc: ActorRef, uri: Uri, sessionUri: SessionUri, agentL: AgentUri, agentR: AgentUri, cnxnLabel: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[Connection] = {
    val sourceUri: String                = makeAliasUri(agentL)
    val targetUri: String                = makeAliasUri(agentR)
    val cont: EstablishConnectionRequest = EstablishConnectionRequest(sessionUri, sourceUri, targetUri, cnxnLabel)
    post(hc, uri, cont).map((response: HttpResponse) => Connection(sourceUri, targetUri, cnxnLabel))
  }

  def makePost(hc: ActorRef, uri: Uri, sessionUri: SessionUri, targets: List[Connection], label: String, uid: String, value: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[Connection] = {
    val from: SessionUri           = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: targets, label, Some(value), Some(uid))
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("insertContent", cont))
    post(hc, uri, req).map((response: HttpResponse) => selfcnxn)
  }

  def makeQueryOnConnections(hc: ActorRef, uri: Uri, sessionUri: SessionUri, connections: List[Connection], lbl: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[Unit] = {
    val from: SessionUri           = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: connections, lbl, None, None)
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("feedExpr", cont))
    post(hc, uri, req).map((response: HttpResponse) => ())
  }

  def makeQueryOnSelf(hc: ActorRef, uri: Uri, sessionUri: SessionUri, label: String)(implicit ec: ExecutionContext,
                                                                                     timeout: Timeout): Future[Connection] = {
    val from: SessionUri           = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: Nil, label, None, None)
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("feedExpr", cont))
    post(hc, uri, req).map((response: HttpResponse) => selfcnxn)
  }

  def getConnectionProfiles(hc: ActorRef, uri: Uri, sessionId: SessionUri)(implicit ec: ExecutionContext,
                                                                           timeout: Timeout): Future[String] =
    post(hc, uri, GetConnectionProfiles(sessionId)).map { (response: HttpResponse) =>
      response.entity.asString
    }

  def sessionPing(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] =
    post(hc, uri, SessionPing(sessionUri)).map { (response: HttpResponse) =>
      parseHttpResponseEntity(response).extract[JArray]
    }

  def pingUntilPong(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] = {
    @scala.annotation.tailrec
    def _step(acc: List[JValue]): List[JValue] = {
      val ja = Await.result(sessionPing(hc, uri, sessionUri), FiniteDuration(10, SECONDS))
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

  class Pingerator(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout)
      extends Iterator[JArray] {

    private def isPong(jValue: JValue): Boolean = (jValue \ "msgType").extract[String] == "sessionPong"

    private var ponged = false

    override def hasNext: Boolean = !ponged

    override def next(): JArray = {
      val jArray: JArray = Await.result(sessionPing(hc, uri, sessionUri), FiniteDuration(10, SECONDS))
      ponged = jArray.arr.exists(isPong)
      jArray
    }
  }

  def altPingUntilPong(hc: ActorRef, uri: Uri, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] =
    Future {
      new Pingerator(hc, uri, sessionUri).fold(JArray(Nil)) { (left: JArray, right: JArray) =>
        JArray(left.arr ++ right.arr)
      }
    }
}
