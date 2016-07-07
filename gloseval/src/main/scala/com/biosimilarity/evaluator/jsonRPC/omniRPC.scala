package com.biosimilarity.evaluator.omniRPC

import java.util.UUID

import com.biosimilarity.evaluator.distribution._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

import scala.math.BigDecimal
import scalaj.http.Http

trait request {}
case class jsonRequest ( method: String, params: JArray, id: String ) extends request
case class jsonNotify( method: String, params: JArray ) extends request


trait result {}
case class Result( result: JValue ) extends result
trait err {}
case class Err( code: Int, message: String, data: Option[JValue]) extends err

//trait response {}
//case class jsonResponse( Result: JValue, Error: JValue, id: String) extends response


case class omniBalance( balance : BigDecimal, reserved : BigDecimal ) {
  def toJson() = {
    "{\"balance\": " + balance + ", \"reserved\": " + reserved + "}"
  }
}

object OmniClient extends EvalConfig
  //with OmniConfig
  with Serializable {

  implicit val formats = org.json4s.DefaultFormats

  private val RPC_USER = EvalConfConfig.read("OmniRPCUser")
  private val RPC_PWD = EvalConfConfig.read("OmniRPCPass")
  private val AMP_PROP_ID = 39
  private val OMNI_URI = EvalConfConfig.read("OmniRPCURI")
  val testAmpAddress = "mfiScEupUknzvkCwDbEEPcjCTiRw17k42X"

  def omniError(reason : String ) : JObject = {
    ("msgType" -> "omniError") ~ ("content" -> ("reason" -> reason) )
  }

  def isOmniError(jo : JValue) : Boolean = {
    (jo \ "msgType").extract[Option[String]] match {
      case Some(s) => s == "omniError"
      case None => false
    }
  }

  private def omniCall(method: String, params: JValue* ): JValue = {
    println(s"omniCall: ${method}")
    val id = UUID.randomUUID().toString

    val requestBody = write(jsonRequest(method, params, id))

    val req = Http(OMNI_URI)
      .timeout(1000, 60000)
      .auth(RPC_USER, RPC_PWD)
      .header("Content-Type", "application/json")
      .postData(requestBody)

    var response = ""
    try {
      response = req.asString.body
    } catch {
      case (e) => {
        return omniError("Unable to connect to Omni server")
      }
    }

    println(s"omni RESPONSE BODY: ${response}")
    if (response.startsWith("Malformed request")) omniError(response)
    else {
      // verify id matches??
      val jsrsp = parse(response).extract[JObject]
      if (id != (jsrsp \ "id").extract[String]) omniError("Invalid response")
      else {
        val erropt = (jsrsp \ "error").extract[Option[JObject]]
        erropt match {
          case None => (jsrsp \ "result")
          case Some(jo) => omniError((jo \ "message").extract[String])
        }
      }
    }
  }

  /*  not currently used
  private def omniNotify(method: String, params: JValue* ) : Unit = {
    println(s"omniNotify: ${method}")

    val requestBody = write(jsonNotify(method, params))

    val req = Http(OMNI_URI)
      .timeout(1000, 60000)
      .auth(RPC_USER, RPC_PWD)
      .header("Content-Type", "application/json")
      .postData(requestBody)
  }
  */

  def getBalance(addr: String) : omniBalance = {
    val rslt = omniCall("omni_getbalance", JString(addr), JInt(AMP_PROP_ID))
    val bal = BigDecimal( (rslt \ "balance").extract[String] )
    val rsv = BigDecimal( (rslt \ "reserved").extract[String] )
    omniBalance(bal,rsv)
  }

  def getBalanceResponse(addr: String) : JValue = {
    val rslt = omniCall("omni_getbalance", JString(addr), JInt(AMP_PROP_ID))
    if (isOmniError(rslt)) rslt
    else {
      val bal = BigDecimal((rslt \ "balance").extract[String])
      val rsv = BigDecimal((rslt \ "reserved").extract[String])
      ("msgType" -> "omniGetBalanceResponse") ~
        ("content" -> omniBalance(bal, rsv).toJson)
    }
  }


  def transfer(fromaddress: String, toaddress: String, amount: BigDecimal) : String = {
    omniCall("omni_send", JString(fromaddress), JString(toaddress), JInt(AMP_PROP_ID), JString(amount.toString())).extract[String]
  }

  def getNewAddress() : String = {
    omniCall("getnewaddress").extract[String]
  }

  def dumpPrivKey(addr : String) : String = {
    omniCall("dumpprivkey", JString(addr)).extract[String]
  }

  def canConnect() : Boolean = {
    !isOmniError( omniCall("omni_getinfo") )
  }


  def runTests() : Unit = {
    val rsp = pretty( omniCall("omni_getinfo") )
    println(rsp)

    val tgt = getNewAddress()
    println( "Private Key : "+ dumpPrivKey(tgt) )

    val tgtbal = getBalance(tgt)

    val orgbal = getBalance(testAmpAddress)
    println( "Private Key : "+ dumpPrivKey(testAmpAddress) )
    //println( orgbal )

    val txn = transfer(testAmpAddress, tgt, orgbal.balance - orgbal.reserved )

    val newbal = getBalance(testAmpAddress)

    println( newbal )

    //println(rsp2)
  }

}

