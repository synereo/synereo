package com.biosimilarity.evaluator.omni

import java.util.UUID

import com.biosimilarity.evaluator.distribution._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

import scala.math.BigDecimal
import scalaj.http.Http

trait Request
case class JsonRequest(method: String, params: JArray, id: String) extends Request
case class JsonNotify(method: String, params: JArray)              extends Request

trait Result
case class Ok(result: JValue)                                    extends Result
case class Err(code: Int, message: String, data: Option[JValue]) extends Result

case class OmniBalance(balance: BigDecimal, reserved: BigDecimal) {
  def toJson = s"""{"balance": $balance, "reserved": $reserved}"""
}

object OmniClient extends EvalConfig with Serializable {

  implicit val formats = org.json4s.DefaultFormats

  private val RPC_USER    = EvalConfigWrapper.readString("OmniRPCUser")
  private val RPC_PWD     = EvalConfigWrapper.readString("OmniRPCPass")
  private val AMP_PROP_ID = 39
  private val OMNI_URI    = EvalConfigWrapper.readString("OmniRPCURI")
  val testAmpAddress      = "mfiScEupUknzvkCwDbEEPcjCTiRw17k42X"

  def omniError(reason: String): JObject = {
    ("msgType" -> "omniError") ~ ("content" -> ("reason" -> reason))
  }

  def isOmniError(jo: JValue): Boolean = {
    (jo \ "msgType").extract[Option[String]] match {
      case Some(s) => s == "omniError"
      case None    => false
    }
  }

  private def omniCall(method: String, params: JValue*): JValue = {
    println(s"omniCall: $method")
    val id = UUID.randomUUID().toString

    val requestBody = write(JsonRequest(method, params, id))

    val req = Http(OMNI_URI).timeout(1000, 60000).auth(RPC_USER, RPC_PWD).header("Content-Type", "application/json").postData(requestBody)

    var response = ""
    try {
      response = req.asString.body
    } catch {
      case e: Throwable => return omniError("Unable to connect to Omni server")
    }

    println(s"omni RESPONSE BODY: $response")
    if (response.startsWith("Malformed request")) omniError(response)
    else {
      // verify id matches??
      val jsrsp = parse(response).extract[JObject]
      if (id != (jsrsp \ "id").extract[String]) omniError("Invalid response")
      else {
        val erropt = (jsrsp \ "error").extract[Option[JObject]]
        erropt match {
          case None     => jsrsp \ "result"
          case Some(jo) => omniError((jo \ "message").extract[String])
        }
      }
    }
  }

  // unused
  private def omniNotify(method: String, params: JValue*): Unit = {
    println(s"omniNotify: $method")
    val requestBody = write(JsonNotify(method, params))
    Http(OMNI_URI).timeout(1000, 60000).auth(RPC_USER, RPC_PWD).header("Content-Type", "application/json").postData(requestBody)
  }

  def getBalance(addr: String): OmniBalance = {
    val rslt = omniCall("omni_getbalance", JString(addr), JInt(AMP_PROP_ID))
    val bal  = BigDecimal((rslt \ "balance").extract[String])
    val rsv  = BigDecimal((rslt \ "reserved").extract[String])
    OmniBalance(bal, rsv)
  }

  def getBalanceResponse(addr: String): JValue = {
    val rslt = omniCall("omni_getbalance", JString(addr), JInt(AMP_PROP_ID))
    if (isOmniError(rslt)) rslt
    else {
      val bal = BigDecimal((rslt \ "balance").extract[String])
      val rsv = BigDecimal((rslt \ "reserved").extract[String])
      ("msgType"   -> "omniGetBalanceResponse") ~
        ("content" -> OmniBalance(bal, rsv).toJson)
    }
  }

  def transfer(fromaddress: String, toaddress: String, amount: BigDecimal): String =
    omniCall("omni_send", JString(fromaddress), JString(toaddress), JInt(AMP_PROP_ID), JString(amount.toString())).extract[String]

  def getNewAddress(): String = omniCall("getnewaddress").extract[String]

  def dumpPrivKey(addr: String): String = omniCall("dumpprivkey", JString(addr)).extract[String]

  def canConnect(): Boolean = !isOmniError(omniCall("omni_getinfo"))

  def runTests(): Unit = {

    val rsp = pretty(omniCall("omni_getinfo"))

    println(rsp)

    val tgt = getNewAddress()

    println(s"Private Key: ${dumpPrivKey(tgt)}")

    val tgtbal = getBalance(tgt)

    val orgbal = getBalance(testAmpAddress)

    println(s"Private Key: ${dumpPrivKey(testAmpAddress)}")

    val txn = transfer(testAmpAddress, tgt, orgbal.balance - orgbal.reserved)

    val newbal = getBalance(testAmpAddress)

    println(newbal)

  }
}
