package com.biosimilarity.evaluator.spray.util

import com.biosimilarity.evaluator.distribution.EvalConfConfig
import spray.http.{HttpHeader, HttpHeaders, StatusCodes, Uri}
import spray.routing.{Directive0, RequestContext}
import spray.routing.Directives._

trait HttpsDirectives {

  import HttpsDirectives._

  def redirectToHttps: Directive0 = requestUri.flatMap { (uri: Uri) =>
    redirect(uri.withScheme("https").withPort(EvalConfConfig.serverSSLPort), StatusCodes.MovedPermanently)
  }

  def requireHttps: Directive0 =
    respondWithHeader(StrictTransportSecurity) &
      extract(isHttpsRequest).flatMap { (b: Boolean) =>
        if (b) pass
        else redirectToHttps
      }

  def requireHttpsIf(p: => Boolean): Directive0 =
    if (p) requireHttps
    else pass
}

object HttpsDirectives {

  val StrictTransportSecurity = HttpHeaders.RawHeader("Strict-Transport-Security", "max-age=31536000")

  def isHttpsRequest(ctx: RequestContext): Boolean =
    ctx.request.uri.scheme == "https" || ctx.request.headers.exists { (h: HttpHeader) =>
      h.is("x-forwarded-proto") && h.value == "https"
    }
}
