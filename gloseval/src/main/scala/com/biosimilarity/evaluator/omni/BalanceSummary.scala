package com.biosimilarity.evaluator.omni

import com.biosimilarity.evaluator.spray.SessionManager
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

case class BalanceSummary(address: String, amp: String, btc: String, errors: List[String]) {
  def cometMessage(session: String) = {
    println(this)
    val content =
      ("sessionURI" -> session) ~
        ("address" -> address) ~
        ("amp" -> amp) ~
        ("btc" -> btc) ~
        ("errors" -> errors)
    SessionManager.cometMessage(session, compact(render(
      ("msgType" -> "balanceChanged") ~ ("content" -> content))))
  }

  override def toString = {
    val errStr = errors match {
      case Nil => "none"
      case list => list.mkString(", ")
    }
    s"*** Balance summary for $address ***" +
      s"\n  balance, AMP: $amp" +
      s"\n  balance, BTC: $btc" +
      s"\n  errors: $errStr\n"
  }
}
