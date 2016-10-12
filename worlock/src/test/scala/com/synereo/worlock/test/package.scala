package com.synereo.worlock

import java.security.cert.X509Certificate
import javax.net.ssl.{KeyManager, SSLContext, TrustManager, X509TrustManager}

import akka.actor.{ActorRef, ActorSystem, Scheduler}
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.spray.client.ApiClient
import spray.http.HttpMethods.GET
import spray.http.{HttpRequest, HttpResponse, Uri}
import spray.io.{ClientSSLEngineProvider, SSLContextProvider}

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

package object test extends ApiClient {

  def optionToTry[T](option: Option[T]): Try[T] =
    option match {
      case Some(x) => Success(x)
      case None    => Failure(new Exception)
    }

  def retry[T](op: => Future[T], delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext, s: Scheduler): Future[T] =
    op.recoverWith {
      case _: Throwable if retries > 0 =>
        akka.pattern.after(delay, s)(retry(op, delay, retries - 1))
    }

  def spinwaitOnServer(system: ActorSystem, uri: Uri, delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext,
                                                                                           s: Scheduler,
                                                                                           t: Timeout): HttpResponse = {
    val e: Future[HttpResponse] = eventualHostConnector(system, 9876, trustfulClientSSLEngineProvider).flatMap { (hc: ActorRef) =>
      retry(hc.ask(HttpRequest(GET, uri)).mapTo[HttpResponse], delay, retries)
    }
    Await.result(e, delay * (retries + 1))
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
