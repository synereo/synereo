package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.Api
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.distribution.EvalConfConfig._
import com.biosimilarity.evaluator.spray.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import com.biosimilarity.evaluator.spray.util._
import org.json4s.{BuildInfo ⇒ _, _}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ApiSpec extends WordSpec with Matchers with BeforeAndAfterEach with ScalaFutures with IntegrationPatience with CapUtilities {

  val system: ActorSystem = ActorSystem()
  val timeout: Timeout = Timeout(60.seconds)

  import system.dispatcher

  implicit val formats = org.json4s.DefaultFormats

  var serverInstance: Option[Server] = None

  // Set up the Spray client's "Host-level" API for doing requests over TLS
  val eventualHostConnector: Future[ActorRef] = IO(Http)(system)
    .ask(Http.HostConnectorSetup("localhost", port = serverSSLPort, sslEncryption = true)(system, clientSSLEngineProvider))(timeout)
    .mapTo[Http.HostConnectorInfo]
    .map((hci: Http.HostConnectorInfo) => hci.hostConnector)

  override def beforeEach(): Unit = {
    resetMongo()
    serverInstance = Some(Server().start())
    Thread.sleep(2000)
  }

  override def afterEach(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }

  type SessionUri = String

  def makePost(cont: Api.RequestContent): HttpRequest = {
    val body = write(cont.asRequest)
    val requestBody: HttpEntity = HttpEntity(ContentType(MediaTypes.`application/json`), body)
    HttpRequest(POST, "/api", entity = requestBody)
  }

  def post(cont: Api.RequestContent): Future[HttpResponse] = {
    val req: HttpRequest = makePost(cont)
    eventualHostConnector
      .flatMap((hc: ActorRef) => hc.ask(req)(timeout))
      .mapTo[HttpResponse]
  }

  def getAgentURI(email: String, password: String): Future[AgentUri] =
    post(Api.GetAgentRequest(email, password)).map { (resp: HttpResponse) =>
      val jsv: JValue = parse(resp.entity.asString)
      val tmsg: String = (jsv \ "msgType").extract[String]
      if (tmsg == "getAgentError") {
        throw new Exception("create user failed, reason : " + (jsv \ "content" \ "reason").extract[String])
      }
      else {
        (jsv \ "content" \ "agentURI").extract[String]
      }
    }

  def createSRPUser(email: String, username: String, pwd: String): Future[AgentUri] = {
    val srpClient = new SRPClient()
    srpClient.init
    val blob = JObject(("name", JString(username)) :: Nil)
    val nce = s"noConfirm:$email"
    for {
      resp1 <- post(Api.CreateUserStep1Request(nce))
      resp2 <- post(Api.CreateUserStep2Request(nce, {
        parse(resp1.entity.asString).extract[Api.ApiResponse].responseContent match {
          case Api.CreateUserStep1Response(salt) =>
            srpClient.calculateX(email, pwd, salt)
            salt
        }}, srpClient.generateVerifier, blob))
    } yield parse(resp2.entity.asString).extract[Api.ApiResponse].responseContent match {
      case Api.CreateUserStep2Response(agentURI) => agentURI
    }
  }

  def openSRPSession(email: String, pwd: String): Future[SessionUri] = {
    val srpClient = new SRPClient()
    srpClient.init
    for {
      agentUri <- getAgentURI(email, pwd)
      resp1 <- post(Api.InitializeSessionStep1Request("%s?A=%s".format(agentUri, srpClient.calculateAHex)))
      resp2 <- post(Api.InitializeSessionStep2Request("%s?M=%s".format(agentUri, srpClient.calculateMHex {
        parse(resp1.entity.asString).extract[Api.ApiResponse].responseContent match {
          case Api.InitializeSessionStep1Response(s, b) =>
            srpClient.calculateX(email, pwd, s)
            b
        }})))
    } yield parse(resp2.entity.asString).extract[Api.ApiResponse].responseContent match {
      case Api.InitializeSessionResponse(sessionURI, m2) if srpClient.verifyServerEvidenceMessage(fromHex(m2)) =>
        sessionURI
    }
  }

  def spawnSession(ssn: SessionUri): Future[SessionUri] = {
    val cont = Api.SpawnSessionRequest(ssn)
    post(cont).map((response: HttpResponse) => {
      val ent = response.entity.asString
      val rsp = parse(ent)
      val newssn = (rsp \ "content" \ "sessionURI").extract[SessionUri]
      newssn
    })
  }

  def openAdminSession() = openSRPSession(readString("nodeAdminEmail"), readString("nodeAdminPass"))

  type AgentUri = String

  def capFromAgentUri(agent: AgentUri) = {
    agent.replace("agent://cap/", "").slice(0, 36)
  }

  def makeAliasUri(agent: AgentUri) = {
    val cap = capFromAgentUri(agent)
    s"alias://$cap/alias"
  }

  def makeConnection(sessionId: SessionUri, agentL: AgentUri, agentR: AgentUri, cnxnLabel: String): Future[Api.Connection] = {
    val sourceUri = makeAliasUri(agentL)
    val targetUri = makeAliasUri(agentR)
    val cont = Api.EstablishConnectionRequest(sessionId, sourceUri, targetUri, cnxnLabel)
    post(cont).map((response: HttpResponse) => Api.Connection(sourceUri, targetUri, cnxnLabel))
  }

  def makePost(sessionId: SessionUri, tgts: List[Api.Connection], lbl: String, uid: String, value: String): Future[Api.Connection] = {
    val from = "agent://" + capFromSession(sessionId)
    val selfcnxn = Api.Connection(from, from, "alias")
    val cont = Api.EvalSubscribeContent(selfcnxn :: tgts, lbl, value, uid)
    val req = Api.EvalSubscribeRequest(sessionId, Api.EvalSubscribeExpression("insertContent", cont))
    post(req).map((response: HttpResponse) =>  selfcnxn)
  }

  def makeQueryOnSelf(sessionId: SessionUri, lbl: String ): Future[Api.Connection] = {
    val from = "agent://" + capFromSession(sessionId)
    val selfcnxn = Api.Connection(from, from, "alias")
    val cont = Api.EvalSubscribeContent(selfcnxn :: Nil, lbl, "", "")
    val req = Api.EvalSubscribeRequest(sessionId, Api.EvalSubscribeExpression("feedExpr", cont))
    post(req).map((response: HttpResponse) => selfcnxn)
  }

  def getConnectionProfiles(sessionId: SessionUri) : Future[String] = {
    val cont = Api.GetConnectionProfiles(sessionId)
    post(cont).map((response: HttpResponse) =>  response.entity.asString)
  }

  def sessionPing(ssn: SessionUri): Future[JArray] = {
    val cont = Api.SessionPing(ssn)
    post(cont).map((response: HttpResponse) => parse(response.entity.asString).extract[JArray])
  }

  def pingUntilPong(ssn: SessionUri): Future[JArray] = {
    def _step(acc: List[JValue]): List[JValue] = {
      val ja = Await.result(sessionPing(ssn), 10.seconds )
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

  "A versionInfoRequest" should {
    "result in a versionInfoResponse" in {
      val eventualResponse: Future[Api.AltResponse[Api.VersionInfoResponse]] =
        post(Api.VersionInfoRequest).map { (response: HttpResponse) =>
          read[Api.AltResponse[Api.VersionInfoResponse]](response.entity.asString)
        }
      whenReady(eventualResponse) { (msg: Api.AltResponse[Api.VersionInfoResponse]) =>
        msg.msgType shouldBe "versionInfoResponse"
        msg.content.glosevalVersion should equal (BuildInfo.version)
        msg.content.scalaVersion should equal (BuildInfo.scalaVersion)
        msg.content.mongoDBVersion should equal (mongoVersion().getOrElse("n/a"))
        msg.content.rabbitMQVersion should equal (rabbitMQVersion().getOrElse("n/a"))
      }
    }
  }

  "The Administrator" should {
    "be able to create a session" in {
      openAdminSession().futureValue shouldNot be ("")
    }

    """query empty database without crashing""" in {
      val proc: Future[(JArray)] = for {
        ssn <- openAdminSession()
        cnxn <- makeQueryOnSelf(ssn, "each([MESSAGEPOSTLABEL])")
        spwnssnA <- spawnSession(ssn)
        a <- sessionPing(spwnssnA)
      } yield a
      proc.futureValue.values.length shouldBe 1
    }
  }




  """The Administrator Session""".stripMargin should {
    """establish the correct number of connections""" in {
      val proc: Future[(JArray, JArray, JArray)] = for {
        ssn <- openAdminSession()
        alice <- createSRPUser("alice@test.com", "alice", "a")
        bob <- createSRPUser("bob@test.com", "bob", "b")
        carol <- createSRPUser("carol@test.com", "carol", "c")

        _ <- makeConnection(ssn, alice, bob, "alice_bob")
        _ <- makeConnection(ssn, alice, carol, "alice_carol")

        ssnA <- openSRPSession("alice@test.com", "a")
        spwnssnA <- spawnSession(ssnA)
        _ <- getConnectionProfiles(spwnssnA)
        a <- sessionPing(spwnssnA)

        ssnB <- openSRPSession("bob@test.com", "b")
        spwnssnB <- spawnSession(ssnB)
        _ <- getConnectionProfiles(spwnssnB)
        b <- sessionPing(spwnssnB)

        ssnC <- openSRPSession("carol@test.com", "c")
        spwnssnC <- spawnSession(ssnC)
        _ <- getConnectionProfiles(spwnssnC)
        c <- sessionPing(spwnssnC)

      } yield (a, b, c)

      whenReady(proc, timeout(Span(60, Seconds))) {
        case (ja: JArray, jb: JArray, jc: JArray) =>
          //println("Alice's connections: "+pretty(render(ja)))
          //println("Bob's connections: "+pretty(render(jb)))
          //println("Carol's connections: "+pretty(render(jc)))
          ja.values.length shouldBe 3
          jb.values.length shouldBe 2
          jc.values.length shouldBe 2
        case _ => fail("should not happen")
      }
    }

    """return evalSubscribeResponse when querying using any, each or all""" in {
      val proc: Future[(JArray)] = for {
        tssn <- openAdminSession()
        ssn <- spawnSession(tssn)
        _ <- {
          // uncomment the following line allow the test to run faster
          //for (actor <- SessionManager.getSession(ssn) ) actor ! SetPongTimeout(1.seconds)
          makeQueryOnSelf(ssn, "each([MESSAGEPOSTLABEL])")
          makeQueryOnSelf(ssn, "any([MESSAGEPOSTLABEL])")
          makeQueryOnSelf(ssn, "all([MESSAGEPOSTLABEL])")
        }
        a <- sessionPing(ssn)
      } yield a
      whenReady(proc) {
        case (ja: JArray) =>
          ja.arr.length shouldBe 1
          val rsp = ja.arr.head.asInstanceOf[JObject]
          val msgType = (rsp \ "msgType").extract[String]
          msgType shouldBe "evalSubscribeResponse"
        case _ => fail("should not happen")
      }
    }

    """return evalSubscribeError when querying not using any, each or all""" in {
      val proc: Future[(JArray)] = for {
        tssn <- openAdminSession()
        ssn <- spawnSession(tssn)
        _ <- {
          // uncomment the following line to allow the test to run faster
          //for (actor <- SessionManager.getSession(ssn) ) actor ! SetPongTimeout(1.seconds)
          makeQueryOnSelf(ssn, "lordfarquad([MESSAGEPOSTLABEL])")
        }
        //a <- sessionPing(ssn)
        a <- pingUntilPong(ssn)
      } yield a
      whenReady(proc) {
        case (ja: JArray) =>
          ja.arr.length shouldBe 1
          val rsp = ja.arr.head.asInstanceOf[JObject]
          val msgType = (rsp \ "msgType").extract[String]
          msgType shouldBe "evalSubscribeError"
        case _ => fail("should not happen")
      }
    }
  }
}

