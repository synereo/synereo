package com.biosimilarity.evaluator.spray.client

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.spray.CapUtilities
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import spray.can.Http
import spray.http.HttpMethods.POST
import spray.http._
import spray.io.ClientSSLEngineProvider

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

  def toHttpRequest(uri: Uri, requestContent: RequestContent): HttpRequest =
    HttpRequest(POST, uri, entity = HttpEntity(ContentType(MediaTypes.`application/json`), write(requestContent.asRequest)))

  def httpPost(hc: ActorRef, uri: Uri, requestContent: RequestContent)(implicit timeout: Timeout): Future[HttpResponse] =
    hc.ask(toHttpRequest(uri, requestContent)).mapTo[HttpResponse]

  def parseHttpResponseEntity(response: HttpResponse): JValue =
    parse(response.entity.asString)

  def extractResponseContentFromJValue(jValue: JValue): ResponseContent =
    jValue.extract[Response].extractResponseContent

  def extractResponseContentFromHttpResponse: (HttpResponse) => ResponseContent =
    parseHttpResponseEntity _ andThen extractResponseContentFromJValue

  def processCreateUserStep1Response(response: HttpResponse, srpClient: SRPClient, email: String, password: String)(
      implicit ec: ExecutionContext): Future[String] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case CreateUserStep1Response(salt) =>
          srpClient.calculateX(email, password, salt)
          salt
        case _ =>
          throw new Exception(s"A CreateUserStep1Request for $email returned an unexpected response: $response")
      }
    }

  def processCreateUserStep2Response(response: HttpResponse, email: String)(implicit ec: ExecutionContext): Future[String] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case CreateUserStep2Response(agentURI) =>
          agentURI
        case _ =>
          throw new Exception(s"A CreateUserStep2Request for $email returned an unexpected response: $response")
      }
    }

  def eventualSRPClient()(implicit ec: ExecutionContext): Future[SRPClient] =
    Future {
      val s: SRPClient = new SRPClient()
      s.init
      s
    }

  def createTestUsers(hc: ActorRef, uri: Uri, n: Int)(implicit ec: ExecutionContext, timeout: Timeout): Future[List[String]] = {
    val evusers: List[Future[String]] = (0 to n).toList.map { (i: Int) =>
      createSRPUser(hc, uri, s"$i@testing", s"user_$i", i.toString)
    }
    Future.sequence(evusers)
  }

  def createSRPUser(hc: ActorRef, uri: Uri, email: String, username: String, password: String)(implicit ec: ExecutionContext,
                                                                                               timeout: Timeout): Future[String] = {
    createUser(hc, uri, email, password, JObject(("name", JString(username)) :: Nil))
  }

  def createUser(hc: ActorRef, uri: Uri, email: String, password: String, blob: JObject)(implicit ec: ExecutionContext,
                                                                                         timeout: Timeout): Future[String] =
    for {
      srpClient <- eventualSRPClient()
      nce       <- Future(s"noConfirm:$email")
      resp1     <- httpPost(hc, uri, CreateUserStep1Request(nce))
      salt      <- processCreateUserStep1Response(resp1, srpClient, email, password)
      resp2     <- httpPost(hc, uri, CreateUserStep2Request(nce, salt, srpClient.generateVerifier, blob))
      resp3     <- processCreateUserStep2Response(resp2, email)
    } yield resp3

  def processInitializeSessionStep1Response(response: HttpResponse, srpClient: SRPClient, email: String, password: String)(
      implicit ec: ExecutionContext): Future[String] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case InitializeSessionStep1Response(s, b) =>
          srpClient.calculateX(email, password, s)
          b
        case InitializeSessionError(reason) =>
          throw new Exception(s"An InitializeSessionStep1Request for $email returned an error: $reason")
        case _ =>
          throw new Exception(s"An InitializeSessionStep1Request for $email returned an unexpected response: $response")
      }
    }

  def processInitializeSessionResponse(response: HttpResponse, srpClient: SRPClient, email: String)(
      implicit ec: ExecutionContext): Future[InitializeSessionResponse] =
    Future {
      extractResponseContentFromHttpResponse(response) match {
        case isr @ InitializeSessionResponse(_, _, _, _, _, _, _, m2) if srpClient.verifyServerEvidenceMessage(fromHex(m2)) =>
          isr
        case InitializeSessionError(reason) =>
          throw new Exception(s"An InitializeSessionStep2Request for $email returned an error: $reason")
        case _ =>
          throw new Exception(s"An InitializeSessionStep2Request for $email returned an unexpected response: $response")
      }
    }

  def openSRPSession(hc: ActorRef, uri: Uri, email: String, password: String)(implicit ec: ExecutionContext,
                                                                              timeout: Timeout): Future[InitializeSessionResponse] =
    for {
      srpClient <- eventualSRPClient()
      resp1     <- httpPost(hc, uri, InitializeSessionStep1Request("agent://email/%s?A=%s".format(email, srpClient.calculateAHex)))
      b         <- processInitializeSessionStep1Response(resp1, srpClient, email, password)
      resp2     <- httpPost(hc, uri, InitializeSessionStep2Request("agent://email/%s?M=%s".format(email, srpClient.calculateMHex(b))))
      resp3     <- processInitializeSessionResponse(resp2, srpClient, email)
    } yield resp3

  def spawnSession(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    httpPost(hc, uri, SpawnSessionRequest(sessionUri)).map { (response: HttpResponse) =>
      (parseHttpResponseEntity(response) \ "content" \ "sessionURI").extract[String]
    }

  def closeSession(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    httpPost(hc, uri, CloseSessionRequest(sessionUri)).map { (response: HttpResponse) =>
      response.entity.asString
    }

  def startCam(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    httpPost(hc, uri, StartSessionRecording(sessionUri)).map((response: HttpResponse) => sessionUri)

  def stopCam(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    httpPost(hc, uri, StopSessionRecording(sessionUri)).map((response: HttpResponse) => response.entity.asString)

  def openAdminSession(hc: ActorRef, uri: Uri, email: String, password: String)(implicit ec: ExecutionContext,
                                                                                timeout: Timeout): Future[InitializeSessionResponse] =
    openSRPSession(hc, uri, email, password)

  def capFromAgentUri(agent: String): String =
    agent.replace("agent://cap/", "").slice(0, 36)

  def makeAliasUri(agent: String, alias: String = "alias"): String =
    s"alias://${capFromAgentUri(agent)}/$alias"

  def makeConnection(hc: ActorRef, uri: Uri, sessionUri: String, agentL: String, agentR: String, cnxnLabel: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[HttpResponse] = {
    val sourceUri: String                = makeAliasUri(agentL)
    val targetUri: String                = makeAliasUri(agentR)
    val cont: EstablishConnectionRequest = EstablishConnectionRequest(sessionUri, sourceUri, targetUri, cnxnLabel)
    httpPost(hc, uri, cont)
  }

  def addAliasLabels(hc: ActorRef, uri: Uri, sessionUri: String, alias: String, labels: List[String])(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[HttpResponse] = {
    val cont = AddAliasLabelsRequest(sessionUri, alias, labels)
    httpPost(hc, uri, cont)
  }

  def makePost(hc: ActorRef, uri: Uri, sessionUri: String, targets: List[Connection], label: String, value: String, uid: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[HttpResponse] = {
    val from: String               = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: targets, label, Some(value), Some(uid))
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("insertContent", cont))
    httpPost(hc, uri, req)
  }

  def makePosts(hc: ActorRef,
                uri: Uri,
                sessionUri: String,
                targets: List[Connection],
                labels: List[String],
                value: List[String],
                uid: List[String])(implicit ec: ExecutionContext, timeout: Timeout): Future[List[HttpResponse]] = {
    val from: String         = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection = Connection(from, from, "alias")
    val eventualPosts: List[Future[HttpResponse]] = (labels, value, uid).zipped.toList.map { (tuple: (String, String, String)) =>
      val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: targets, tuple._1, Some(tuple._2), Some(tuple._3))
      val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("insertContent", cont))
      httpPost(hc, uri, req)
    }
    Future.sequence(eventualPosts)
  }

  def makeQueryOnConnections(hc: ActorRef, uri: Uri, sessionUri: String, connections: List[Connection], label: String)(
      implicit ec: ExecutionContext,
      timeout: Timeout): Future[HttpResponse] = {
    val from: String               = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: connections, label, None, None)
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("feedExpr", cont))
    httpPost(hc, uri, req)
  }

  def makeQueryOnSelf(hc: ActorRef, uri: Uri, sessionUri: String, label: String)(implicit ec: ExecutionContext,
                                                                                 timeout: Timeout): Future[HttpResponse] = {
    val from: String               = "agent://" + capFromSession(sessionUri)
    val selfcnxn: Connection       = Connection(from, from, "alias")
    val cont: EvalSubscribeContent = EvalSubscribeContent(selfcnxn :: Nil, label, None, None)
    val req: EvalSubscribeRequest  = EvalSubscribeRequest(sessionUri, EvalSubscribeExpression("feedExpr", cont))
    httpPost(hc, uri, req)
  }

  def getConnectionProfiles(hc: ActorRef, uri: Uri, sessionId: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[String] =
    httpPost(hc, uri, GetConnectionProfiles(sessionId)).map { (response: HttpResponse) =>
      response.entity.asString
    }

  def extractConnections(jArray: JArray)(implicit ec: ExecutionContext): Future[List[Connection]] =
    Future {
      jArray.arr.filter { (value: JValue) =>
        (value \ "msgType").extract[String] == "connectionProfileResponse"
      }.map { (value: JValue) =>
        val cnxn   = value \ "content" \ "connection"
        val source = (cnxn \ "source").extract[String]
        val target = (cnxn \ "target").extract[String]
        val label  = (cnxn \ "label").extract[String]
        Connection(source, target, label)
      }
    }

  def sessionPing(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] =
    httpPost(hc, uri, SessionPing(sessionUri)).map { (response: HttpResponse) =>
      parseHttpResponseEntity(response).extract[JArray]
    }

  class Pingerator(hc: ActorRef, uri: Uri, sessionUri: String, msgType: String)(implicit ec: ExecutionContext, timeout: Timeout)
      extends Iterator[JArray] {

    private def isMsgType(jValue: JValue): Boolean = (jValue \ "msgType").extract[String] == msgType

    private var done = false

    override def hasNext: Boolean = !done

    override def next(): JArray = {
      val jArray: JArray = Await.result(sessionPing(hc, uri, sessionUri), timeout.duration)
      done = jArray.arr.exists(isMsgType)
      jArray
    }
  }

  def pingUntilCheckConnectionResponse(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext,
                                                                                   timeout: Timeout): Future[JArray] =
    Future {
      new Pingerator(hc, uri, sessionUri, "checkConnectionResponse").fold(JArray(Nil)) { (left: JArray, right: JArray) =>
        JArray(left.arr ++ right.arr)
      }
    }

  def pingUntilPong(hc: ActorRef, uri: Uri, sessionUri: String)(implicit ec: ExecutionContext, timeout: Timeout): Future[JArray] =
    Future {
      new Pingerator(hc, uri, sessionUri, "sessionPong").fold(JArray(Nil)) { (left: JArray, right: JArray) =>
        JArray(left.arr ++ right.arr)
      }
    }
}
