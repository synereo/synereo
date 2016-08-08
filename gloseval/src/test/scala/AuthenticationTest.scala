package com.biosimilarity.evaluator.spray

import org.bouncycastle.crypto.agreement.srp.{SRP6Client, SRP6StandardGroups, SRP6VerifierGenerator}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalatest.{MustMatchers, WordSpec}
import spray.http._
import spray.testkit.ScalatestRouteTest

import scala.util.Try

object AuthenticationTestData {
  import com.biosimilarity.evaluator.spray.srp.ConversionUtils._

  val email = "metaweta+1@gmail.com"
  val password = "qwerty12345"
  val srpClient = new SRP6Client()

  val createUserStep1RequestBody = s"""{"msgType":"createUserStep1Request","content":{"email":"$email"}}"""
  val confirmEmailTokenRequestBody = """{"msgType":"confirmEmailToken","content":{"token":"b08353e9"}}"""

  def getVerifier(salt: String) = {
    val verifierGenerator = new SRP6VerifierGenerator()
    verifierGenerator.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest())
    toHex(verifierGenerator.generateVerifier(salt.getBytes, email.getBytes, password.getBytes))
  }
  def getA(salt: String) = {
    srpClient.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest(), getSecureRandom)
    toHex(srpClient.generateClientCredentials(salt.getBytes, email.getBytes, password.getBytes))
  }

  def getCreateUserStep2RequestBody(salt: String): String =
    s"""{"msgType":"createUserStep2Request","content":{"email":"$email","salt": "$salt","verifier":"${getVerifier(salt)}","jsonBlob":{"name":"test"}}}"""

  def getInitializeSessionStep1RequestBody(salt: String): String =
    s"""{"msgType":"initializeSessionStep1Request","content":{"agentURI":"agent://email/$email?A=${getA(salt)}"}}"""

  def getInitializeSessionStep2RequestBody(bVal: String): String = {
    srpClient.calculateSecret(fromHex(bVal))
    s"""{"msgType":"initializeSessionStep2Request","content":{"agentURI":"agent://email/$email?M=${toHex(srpClient.calculateClientEvidenceMessage())}"}}"""
  }
}

class AuthenticationTest extends WordSpec with MustMatchers with ScalatestRouteTest with EvaluatorService {

  implicit val formats = DefaultFormats

  var salt = ""
  var B = ""

  def actorRefFactory = system

  import AuthenticationTestData._

  "EvaluationService" should {

    "return random salt on creation user, step 1" in {
      Post("/api", HttpEntity(MediaTypes.`application/json`, createUserStep1RequestBody)) ~> myRoute ~>
        check {
          val rsp = responseAs[String]
          rsp must include("salt")

          val json    = parse(rsp)
          val optSalt = Try((json \ "content" \ "salt").extract[String]).toOption
          optSalt must not be None

          salt = optSalt.getOrElse("")
          salt must not be ""
        }
    }

    "respond with CreateUserWaiting on creation user, step 2" in {
      Post("/api", HttpEntity(MediaTypes.`application/json`, getCreateUserStep2RequestBody(salt))) ~> myRoute ~> {
        check {
          val rsp = responseAs[String]
          rsp must include("createUserWaiting")
        }
      }
    }

    "generate agentURI on email token confirmation" in {
      Post("/api", HttpEntity(MediaTypes.`application/json`, confirmEmailTokenRequestBody)) ~> myRoute ~>
        check {
          val rsp     = responseAs[String]
          val json    = parse(rsp)
          val msgType = (json \ "msgType").extract[String]
          msgType mustEqual "createUserStep2Response"

          val agentURI = (json \ "content" \ "agentURI").extract[String]
          agentURI must include("agent://cap/")
        }
    }

    "respond with B parameter and stored salt on initializing new session, step 1" in {
      Post("/api", HttpEntity(MediaTypes.`application/json`, getInitializeSessionStep1RequestBody(salt))) ~> myRoute ~>
        check {
          val rsp     = responseAs[String]
          val json    = parse(rsp)
          val msgType = (json \ "msgType").extract[String]
          msgType mustEqual "initializeSessionStep1Response"

          val s = (json \ "content" \ "s").extract[String]
          s mustEqual salt

          B = (json \ "content" \ "B").extract[String]
          B must not be ""
        }
    }

    "respond with M2 parameter on initializing new session, step 2" in {
      Post("/api", HttpEntity(MediaTypes.`application/json`, getInitializeSessionStep2RequestBody(B))) ~> myRoute ~>
        check {
          val rsp     = responseAs[String]
          val json    = parse(rsp)
          val msgType = (json \ "msgType").extract[String]
          msgType mustEqual "initializeSessionResponse"

          val m2 = (json \ "content" \ "M2").extract[String]
          m2 must not be ""
        }
    }
  }
}
