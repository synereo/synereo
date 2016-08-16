package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.Api
import com.biosimilarity.evaluator.distribution.EvalConfConfig._
import com.biosimilarity.evaluator.spray.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import com.biosimilarity.evaluator.spray.util._
import com.typesafe.config.{Config, ConfigFactory}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import spray.can.Http
import spray.can.server.ServerSettings
import spray.http.HttpMethods._
import spray.http._

import scala.concurrent.Future
import scala.concurrent.duration._

class ApiSpec extends WordSpec with Matchers with BeforeAndAfterEach with ScalaFutures with IntegrationPatience {

  val config: Config = ConfigFactory.load()
  val settings: ServerSettings = ServerSettings(config)
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
    serverInstance = Some(new Server(settings).start())
    Thread.sleep(2000)
  }

  override def afterEach(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }

  def makeRequest(cont: Api.RequestContent): HttpRequest = {
    val body = write(Api.toReq(cont))
    println(body)
    val requestBody: HttpEntity =
      HttpEntity(ContentType(MediaTypes.`application/json`), body)
    HttpRequest(POST, "/api", entity = requestBody)

  }

  type SessionUri = String

  def post(req: HttpRequest): Future[HttpResponse] = {
    eventualHostConnector
      .flatMap((hc: ActorRef) => hc.ask(req)(timeout))
      .mapTo[HttpResponse]
  }

  def post(cont: Api.RequestContent): Future[HttpResponse] = {
    val req = makeRequest(cont)
    post(req)
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

  def makeConnection(sessionId: SessionUri, agentL: AgentUri, agentR: AgentUri, cnxnLabel: String): Future[Api.Connection] = {
    def makeAliasUri(agent: String) = {
        val cap = agent.replace("agent://cap/", "").slice(0, 36)
        s"alias://$cap/alias"
    }
    val sourceUri = makeAliasUri(agentL)
    val targetUri = makeAliasUri(agentR)
    //val cnxnLabel = UUID.randomUUID().toString

    val cont = Api.EstablishConnectionRequest(sessionId, sourceUri, targetUri, cnxnLabel)
    post(cont).map((response: HttpResponse) => {
      //val rsp = response.entity.asString
      Api.Connection(sourceUri, targetUri, cnxnLabel)
    })
  }

  def getConnectionProfiles(sessionId: SessionUri) : Future[String] = {
    val cont = Api.GetConnectionProfiles(sessionId)
    post(cont).map((response: HttpResponse) => {
      val rsp = response.entity.asString
      rsp
    })
  }

  def sessionPing(ssn: SessionUri): Future[JArray] = {
    val cont = Api.SessionPing(ssn)
    post(cont).map((response: HttpResponse) => {
      parse(response.entity.asString).extract[JArray]
    })
  }

  "The Administrator" should {
    "be able to create a session" in {
      openAdminSession().futureValue shouldNot be ("")
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
          println("Alice's connections: "+pretty(render(ja)))
          println("Bob's connections: "+pretty(render(jb)))
          println("Carol's connections: "+pretty(render(jc)))
          ja.values.length shouldBe 3
          jb.values.length shouldBe 2
          jc.values.length shouldBe 2
        case _ => fail("should not happen")
      }
    }
  }
}

