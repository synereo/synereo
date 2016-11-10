package com.synereo.worlock

import java.security.cert.X509Certificate
import javax.net.ssl.{KeyManager, SSLContext, TrustManager, X509TrustManager}

import akka.actor.{ActorRef, ActorSystem, Scheduler}
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.api.CheckConnectionRequest
import com.biosimilarity.evaluator.spray.client.ApiClient
import org.json4s.JValue
import spray.can.Http
import spray.http.HttpMethods.GET
import spray.http.{HttpRequest, HttpResponse, Uri}
import spray.io.{ClientSSLEngineProvider, SSLContextProvider}

import scala.concurrent.blocking
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

package object test extends ApiClient {

  def retry[T](op: => Future[T], delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext, s: Scheduler): Future[T] =
    op.recoverWith {
      case _: Throwable if retries > 0 =>
        akka.pattern.after(delay, s)(retry(op, delay, retries - 1))
    }

  def spinwaitOnServer(system: ActorSystem, uri: Uri, delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext,
                                                                                           s: Scheduler,
                                                                                           t: Timeout): HttpResponse = {
    val connector: Future[ActorRef] = eventualHostConnector(system, uri.effectivePort, trustfulClientSSLEngineProvider)
    try {
      val e: Future[HttpResponse] = connector.flatMap { (hc: ActorRef) =>
        retry(hc.ask(HttpRequest(GET, uri)).mapTo[HttpResponse], delay, retries)
      }
      Await.result(e, delay * (retries + 1))
    } finally {
      connector.map((ref: ActorRef) => ref.ask(Http.CloseAll))
    }
  }

  def quiescenceCheck(system: ActorSystem,
                      apiUri: Uri,
                      spinwaitDelay: FiniteDuration,
                      spinwaitRetries: Integer,
                      checkConnectionDuration: FiniteDuration)(implicit ec: ExecutionContext, s: Scheduler, timeout: Timeout): Boolean = {
    val eventualBoolean: Future[Boolean] =
      for {
        hc        <- eventualHostConnector(system, apiUri.effectivePort, trustfulClientSSLEngineProvider)
        uri       <- Future(apiUri)
        _         <- retry(hc.ask(HttpRequest(GET, uri)).mapTo[HttpResponse], spinwaitDelay, spinwaitRetries)
        _         <- Future(blocking(Thread.sleep(10000L)))
        isr       <- openAdminSession(hc, uri, "admin@localhost", "a")
        _         <- httpPost(hc, uri, CheckConnectionRequest(isr.sessionURI))
        jArray    <- pingUntilCheckConnectionResponse(hc, uri, isr.sessionURI)(ec, checkConnectionDuration)
        connected <- Future(jArray.arr.exists((value: JValue) => (value \ "msgType").extract[String] == "checkConnectionResponse"))
        _         <- hc.ask(Http.CloseAll)
      } yield connected
    Await.result(eventualBoolean, spinwaitDelay * (spinwaitRetries + 1) + checkConnectionDuration)
  }

  private object TrustfulX509TrustManager extends X509TrustManager {

    def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = ()

    def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = ()

    def getAcceptedIssuers: Array[X509Certificate] = Array[X509Certificate]()
  }

  private def trustfulSSLContext: SSLContext = {
    val context: SSLContext = SSLContext.getInstance("TLS")
    context.init(Array.empty[KeyManager], Array[TrustManager](TrustfulX509TrustManager), null)
    context
  }

  def trustfulClientSSLEngineProvider: ClientSSLEngineProvider =
    ClientSSLEngineProvider(identity)(SSLContextProvider.forContext(trustfulSSLContext))
}
