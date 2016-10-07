package com.biosimilarity.evaluator.spray.directives

import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.spray.directives.HttpsDirectives.StrictTransportSecurity
import org.scalatest.{Matchers, WordSpec}
import spray.http.{HttpHeaders, StatusCodes, Uri}
import spray.routing.Directives._
import spray.routing.Route
import spray.testkit.ScalatestRouteTest

class HttpsDirectivesSpec extends WordSpec with HttpsDirectives with ScalatestRouteTest with Matchers {

  val httpUri: Uri  = Uri("http://alpha.synereo.com/api").withPort(EvalConfigWrapper.serverPort)
  val httpsUri: Uri = httpUri.withScheme("https").withPort(EvalConfigWrapper.serverSSLPort)

  "The requireHttps directive" should {

    val route: Route = requireHttps(complete(StatusCodes.OK))

    "redirect plain http requests to https" in {
      Get(httpUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        header[HttpHeaders.Location].map((l: HttpHeaders.Location) => Uri(l.value)) shouldBe Some(httpsUri)
        status should ===(StatusCodes.MovedPermanently)
      }
    }

    "redirect http requests with a `X-Forwarded-Proto` header https" in {
      Get(httpUri) ~> addHeader(HttpHeaders.RawHeader("X-Forwarded-Proto", "http")) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        header[HttpHeaders.Location].map((l: HttpHeaders.Location) => Uri(l.value)) shouldBe Some(httpsUri)
        status should ===(StatusCodes.MovedPermanently)
      }
    }

    "allow https requests and responses with a `StrictTransportSecurity` header" in {
      Get(httpsUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        status should ===(StatusCodes.OK)
      }
    }

    """|respond to https requests with a `X-Forwarded-Proto` header
       |with message containing a `StrictTransportSecurity` header""".stripMargin in {
      Get(httpUri) ~> addHeader(HttpHeaders.RawHeader("X-Forwarded-Proto", "https")) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        status should ===(StatusCodes.OK)
      }
    }
  }

  "The requireHttpsIf directive" should {

    "require https if its argument is true" in {

      val route: Route = requireHttpsIf(true)(complete(StatusCodes.OK))

      Get(httpUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        header[HttpHeaders.Location].map((l: HttpHeaders.Location) => Uri(l.value)) shouldBe Some(httpsUri)
        status should ===(StatusCodes.MovedPermanently)
      }

      Get(httpsUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe Some(StrictTransportSecurity)
        status should ===(StatusCodes.OK)
      }
    }

    "not require https if its argument is false " in {

      val route: Route = requireHttpsIf(false)(complete(StatusCodes.OK))

      Get(httpUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe None
        status should === (StatusCodes.OK)
      }

      Get(httpsUri) ~> route ~> check {
        header(StrictTransportSecurity.name) shouldBe None
        status should === (StatusCodes.OK)
      }
    }
  }
}
