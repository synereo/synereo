package com.biosimilarity.evaluator.spray

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.BuildInfo
import com.biosimilarity.evaluator.api._
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration._
import com.biosimilarity.evaluator.util._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.json4s.{BuildInfo => _, _}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}
import spray.can.Http
import spray.http.Uri
import spray.io.ClientSSLEngineProvider

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

abstract class ImporterTests(val apiUri: Uri, sslEngineProvider: ClientSSLEngineProvider)
  extends WordSpec
    with ApiClient
    with Matchers
    with BeforeAndAfterEach
    with ScalaFutures {

  val system: ActorSystem = ActorSystem()

  implicit val ec = system.dispatcher

  implicit val timeout: Timeout

  val logger: Logger

  "The Importer" should {

    "import the test file " in {

      val eventualResponse: Future[Response] =
        for {
          uri <- Future(apiUri)
          hc <- eventualHostConnector(system, uri.effectivePort, sslEngineProvider)
          uri <- Future("/api")
          httpResponse <- httpPost(hc, uri, VersionInfoRequest)
          response <- Future(read[Response](httpResponse.entity.asString))
          _ <- hc.ask(Http.CloseAll)
        } yield response

      whenReady(eventualResponse) { (response: Response) =>
        response.msgType shouldBe "versionInfoResponse"
        response.extractResponseContent match {
          case VersionInfoResponse(gv, sv, mdbv, rmqv) =>
            gv should equal(BuildInfo.version)
            sv should equal(BuildInfo.scalaVersion)
          case _ =>
            fail("should not happen")
        }
      }
    }

  }
}