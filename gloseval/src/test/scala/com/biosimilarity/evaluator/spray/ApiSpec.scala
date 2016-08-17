package com.biosimilarity.evaluator.spray

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.Api
import com.biosimilarity.evaluator.distribution.EvalConfConfig._
import com.biosimilarity.evaluator.spray.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.CapUtilities
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import spray.can.Http
import spray.can.server.ServerSettings
import spray.http.HttpMethods._
import spray.http._
import com.biosimilarity.evaluator.spray.util._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.Future
import scala.concurrent.duration._

class ApiSpec extends WordSpec with Matchers with BeforeAndAfterAll with ScalaFutures with IntegrationPatience with CapUtilities {

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

  override def beforeAll(): Unit = {
    resetMongo()
    serverInstance = Some(new Server(settings).start())
    Thread.sleep(2000)
  }

  override def afterAll(): Unit = {
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

  def post(cont: Api.RequestContent): Future[HttpResponse] = {
    val req = makeRequest(cont)
    eventualHostConnector
      .flatMap((hc: ActorRef) => hc.ask(req)(timeout))
      .mapTo[HttpResponse]
  }

  def openSession(username: String, password: String): Future[SessionUri] = {
    val agentUri = s"""agent://email/$username?password=$password"""
    val cont = Api.InitializeSessionRequest(agentUri)
    post(cont).map((response: HttpResponse) => {
      val ent = response.entity.asString
      val rsp = parse(ent)
      val isr = (rsp \ "content").extract[Api.InitializeSessionResponse]
      isr.sessionURI
    })
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

  def openAdminSession() = openSession(readString("nodeAdminEmail"), readString("nodeAdminPass"))

  type AgentUri = String

  def createUser(email: String, username: String, password: String): Future[AgentUri] = {
    val blob = JObject(("name", JString(username)) :: Nil)
    val cont = Api.CreateUserRequest("noConfirm:" + email, password, blob)
    post(cont).map((response: HttpResponse) => {
      val rsp = parse(response.entity.asString)
      (rsp \ "content" \ "agentURI").extract[AgentUri]
    })
  }

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
    //val cnxnLabel = UUID.randomUUID().toString

    val cont = Api.EstablishConnectionRequest(sessionId, sourceUri, targetUri, cnxnLabel)
    post(cont).map((response: HttpResponse) => {
      //val rsp = response.entity.asString
      Api.Connection(sourceUri, targetUri, cnxnLabel)
    })
  }

  def makePost(sessionId: SessionUri, tgts: List[Api.Connection], lbl: String, uid: String, value: String): Future[Api.Connection] = {
    val from = "agent://" + capFromSession(sessionId)
    val selfcnxn = Api.Connection(from, from, "alias")

    val cont = Api.EvalSubscribeContent(selfcnxn :: tgts, lbl, value, uid)
    val req = Api.EvalSubscribeRequest(sessionId, Api.EvalSubscribeExpression("insertContent", cont))
    post(req).map((response: HttpResponse) => {
      //val rsp = response.entity.asString
      selfcnxn
    })
  }

  def makeQueryOnSelf(sessionId: SessionUri, lbl: String ): Future[Api.Connection] = {
    val from = "agent://" + capFromSession(sessionId)
    val selfcnxn = Api.Connection(from, from, "alias")

    val cont = Api.EvalSubscribeContent(selfcnxn :: Nil, lbl, "", "")
    val req = Api.EvalSubscribeRequest(sessionId, Api.EvalSubscribeExpression("feedExpr", cont))
    post(req).map((response: HttpResponse) => {
      //val rsp = response.entity.asString
      selfcnxn
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

  /*
  def pingUntilPong(ssn: SessionUri): Future[JArray] = {
    val cont = Api.SessionPing(ssn)
    def _pingRec(cur: List[JValue]) : Future[List[JValue]] = {
      post(cont).map((response: HttpResponse) => {
        val ja = parse(response.entity.asString).extract[JArray]
        var done = false
        var rslt = cur
        ja.values.foreach[JValue]( (jv : JValue) => {
          val typ = (jv \ "msgType").extract[String]
          if (typ == "sessionPong") done = true
          else rslt = jv :: cur
        })
      })
    }
  }
  */

  """The Administrator Session""".stripMargin should {

    """establish the correct number of connections""" in {
      val proc: Future[(JArray, JArray, JArray)] = for {
        ssn <- openAdminSession()
        alice <- createUser("alice@test.com", "alice", "a")
        bob <- createUser("bob@test.com", "bob", "b")
        carol <- createUser("carol@test.com", "carol", "c")

        _ <- makeConnection(ssn, alice, bob, "alice_bob")
        _ <- makeConnection(ssn, alice, carol, "alice_carol")

        ssnA <- openSession("alice@test.com", "a")
        spwnssnA <- spawnSession(ssnA)
        _ <- getConnectionProfiles(spwnssnA)
        a <- sessionPing(spwnssnA)

        ssnB <- openSession("bob@test.com", "b")
        spwnssnB <- spawnSession(ssnB)
        _ <- getConnectionProfiles(spwnssnB)
        b <- sessionPing(spwnssnB)

        ssnC <- openSession("carol@test.com", "c")
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

    """query empty database without crashing""" in {
//      val value = "{\"$type\":\"shared.models.MessagePost\", \"created\" : \"2002-05-30T09:30:10Z\", \"modified\" : \"2002-05-30T09:30:10Z\", \"messagePostContent\": {\"$type\":\"shared.models.MessagePostContent\",\"versionedPostId\" : \"35e60447-747e-496a-afde-65ca182db1c8\", \"versionedPostPredecessorId\" : \"\", \"versionNumber\" : \"1\", \"canForward\" : true, \"text\" : \"this is a subject for the message\",\"subject\" : \"this is a message\"}}"
//      val uid = "0c778b40-4799-4557-9050-fd7a4b77c23e"

      val proc: Future[(JArray)] = for {
        ssn <- openAdminSession()
        cnxn <- makeQueryOnSelf(ssn, "each([MESSAGEPOSTLABEL])")
        spwnssnA <- spawnSession(ssn)
        a <- sessionPing(spwnssnA)

      } yield (a)

      whenReady(proc, timeout(Span(60, Seconds))) {
        case (ja: JArray) =>
          println("Query results: "+pretty(render(ja)))
          ja.values.length shouldBe 1
        case _ => fail("should not happen")
      }
    }
  }
}

