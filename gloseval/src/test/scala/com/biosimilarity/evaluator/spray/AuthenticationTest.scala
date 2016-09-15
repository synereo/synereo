package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution.EvalConfConfig
import com.biosimilarity.evaluator.spray.srp.SRPClient
import com.biosimilarity.evaluator.spray.util._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpec}
import spray.http._
import spray.testkit.ScalatestRouteTest

import scala.util.Try

object AuthenticationTestData {
  val email = "testonly@test.com"
  val password = "qwerty12345"
  val srpClient = new SRPClient()

  val createUserStep1RequestBody = s"""{"msgType":"createUserStep1Request","content":{"email":"$email"}}"""
  val confirmEmailTokenRequestBody = """{"msgType":"confirmEmailToken","content":{"token":"b08353e9"}}"""

  srpClient.init

  def getVerifier(salt: String) = {
    srpClient.calculateX(email, password, salt)
    srpClient.generateVerifier
  }
  def getA = srpClient.calculateAHex

  def getCreateUserStep2RequestBody(salt: String): String =
    s"""{"msgType":"createUserStep2Request","content":{"email":"$email","salt": "$salt","verifier":"${getVerifier(salt)}","jsonBlob":{"name":"test"}}}"""

  def getInitializeSessionStep1RequestBody(salt: String): String =
    s"""{"msgType":"initializeSessionStep1Request","content":{"agentURI":"agent://email/$email?A=$getA"}}"""

  def getInitializeSessionStep2RequestBody(bVal: String): String =
    s"""{"msgType":"initializeSessionStep2Request","content":{"agentURI":"agent://email/$email?M=${srpClient.calculateMHex(bVal)}"}}"""
}

class AuthenticationTest extends WordSpec with BeforeAndAfterAll with MustMatchers with ScalatestRouteTest with EvaluatorService {

  implicit val formats = DefaultFormats

  var salt = ""
  var B = ""

  def actorRefFactory = system

  var serverInstance: Option[Server] = None

  val httpsUri: Uri = Uri("/api").withScheme("https").withPort(EvalConfConfig.serverSSLPort)

  override def beforeAll(): Unit = {
    resetMongo()
    serverInstance = Some(Server().start())
  }

  override def afterAll(): Unit = {
    serverInstance.map(_.stop())
    serverInstance = None
  }

  import AuthenticationTestData._

  "EvaluationService" should {

    "return random salt on creation user, step 1" in {
      Post(httpsUri, HttpEntity(MediaTypes.`application/json`, createUserStep1RequestBody)) ~> myRoute ~>
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

    // TODO: re-enable after we stop using smtp.googlemail.com as our SMTP server for tests
    "respond with CreateUserWaiting on creation user, step 2" ignore {
      Post(httpsUri, HttpEntity(MediaTypes.`application/json`, getCreateUserStep2RequestBody(salt))) ~> myRoute ~> {
        check {
          val rsp = responseAs[String]
          rsp must include("createUserWaiting")
        }
      }
    }

    "generate agentURI on email token confirmation" in {
      Post(httpsUri, HttpEntity(MediaTypes.`application/json`, confirmEmailTokenRequestBody)) ~> myRoute ~>
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
      Post(httpsUri, HttpEntity(MediaTypes.`application/json`, getInitializeSessionStep1RequestBody(salt))) ~> myRoute ~>
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
      Post(httpsUri, HttpEntity(MediaTypes.`application/json`, getInitializeSessionStep2RequestBody(B))) ~> myRoute ~>
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
