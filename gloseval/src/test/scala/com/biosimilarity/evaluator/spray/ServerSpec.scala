package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfConfig._
import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.ClientSSLConfiguration._
import com.biosimilarity.evaluator.spray.util.HttpsDirectives.StrictTransportSecurity
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import spray.can.Http
import spray.can.server.ServerSettings
import spray.http.HttpMethods._
import spray.http._
import com.biosimilarity.evaluator.spray.util._

import scala.concurrent.Future
import scala.concurrent.duration._

class ServerSpec extends WordSpec with Matchers with BeforeAndAfterAll with ScalaFutures with IntegrationPatience {

  val config: Config           = ConfigFactory.load()
  val settings: ServerSettings = ServerSettings(config)
  val system: ActorSystem      = ActorSystem()
  val timeout: Timeout         = Timeout(15.seconds)
  import system.dispatcher

  var serverInstance: Option[Server] = None

  // Set up the Spray client's "Host-level" API for doing requests over TLS
  val eventualHostConnector: Future[ActorRef] = IO(Http)(system)
    .ask(Http.HostConnectorSetup("localhost", port = serverSSLPort, sslEncryption = true)(system, clientSSLEngineProvider))(timeout)
    .mapTo[Http.HostConnectorInfo]
    .map((hci: Http.HostConnectorInfo) => hci.hostConnector)

  override def beforeAll(): Unit = {
    serverInstance = Some(new Server(settings).start())
    resetMongo()
  }

  override def afterAll(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }

  "A GET request sent over http to the '/api' route of a running instance of Server" should {
    """respond with the proper 301 "Moved Permanently" status code and HSTS header""" in {
      val uri: Uri                       = Uri("http://localhost/api").withPort(serverPort)
      val response: Future[HttpResponse] = IO(Http)(system).ask(HttpRequest(GET, uri))(timeout).mapTo[HttpResponse]
      whenReady(response) { (r: HttpResponse) =>
        r.status shouldBe StatusCodes.MovedPermanently
        r.headers should contain(StrictTransportSecurity)
      }
    }
  }

  "A GET request sent over https to the '/api' route of a running instance of Server" should {
    """respond with the proper 405 "Method Not Allowed" status code""" in {
      eventualHostConnector
        .flatMap((hc: ActorRef) => hc.ask(HttpRequest(GET, "/api"))(timeout))
        .mapTo[HttpResponse]
        .map((response: HttpResponse) => response.status)
        .futureValue shouldBe StatusCodes.MethodNotAllowed
    }
  }

  """A POST of msgType 'createUserStep1Request' sent over http
    |to the '/api' route of a running instance of Server""".stripMargin should {
    """respond with the proper 301 "Moved Permanently" status code and HSTS header""" in {
      val email = "testonly@test.com"
      val requestBody: HttpEntity =
        HttpEntity(ContentType(MediaTypes.`application/json`), s"""{"msgType":"createUserStep1Request","content":{"email":"$email"}}""")
      val uri: Uri                       = Uri("http://localhost/api").withPort(serverPort)
      val response: Future[HttpResponse] = IO(Http)(system).ask(HttpRequest(POST, uri, entity = requestBody))(timeout).mapTo[HttpResponse]
      whenReady(response) { (r: HttpResponse) =>
        r.status shouldNot be(StatusCodes.OK)
        r.status shouldBe StatusCodes.MovedPermanently
        r.headers should contain(StrictTransportSecurity)
      }
    }
  }

  """A POST of msgType 'createUserStep1Request' sent over https
    |to the '/api' route of a running instance of Server""".stripMargin should {
    """respond with the proper 200 "OK" status code""" in {
      val email = "testonly@test.com"
      val requestBody: HttpEntity =
        HttpEntity(ContentType(MediaTypes.`application/json`), s"""{"msgType":"createUserStep1Request","content":{"email":"$email"}}""")
      eventualHostConnector
        .flatMap((hc: ActorRef) => hc.ask(HttpRequest(POST, "/api", entity = requestBody))(timeout))
        .mapTo[HttpResponse]
        .map((response: HttpResponse) => response.status)
        .futureValue shouldBe StatusCodes.OK
    }
  }

  "Importer" should {
    "run test files" in {
      Importer.runTestFiles()
    }
  }
}
