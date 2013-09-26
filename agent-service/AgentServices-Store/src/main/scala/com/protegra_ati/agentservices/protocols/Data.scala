package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.ConcreteHL
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.text.SimpleDateFormat
import java.util.{TimeZone, Date}

trait Data {
  val timestamp: Option[Date] = None

  protected def timestampLabel: String = {
    s"timestamp(${generateLabelValue(timestampUTCStr)})"
  }

  protected def generateLabelValue(value: Option[String]): String = {
    value match {
      case Some(v) => "\"" + v + "\""
      case None => "_"
    }
  }

  protected def timestampUTCStr: Option[String] = {
    timestamp match {
      case Some(ts) => {
        val formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
        Some(formatter.format(ts))
      }
      case None => None
    }
  }

  protected def generateLabelStr(): String

  def toCnxnCtxtLabel: CnxnCtxtLabel[String, String, String] = {
    generateLabelStr().toLabel
  }

  def toGround: mTT.Ground = {
    mTT.Ground(ConcreteHL.InsertContent(toCnxnCtxtLabel, Nil, this))
  }
}

case class Cnxns(
    override val timestamp: Option[Date],
    cnxnList: List[(acT.AgentCnxn, acT.AgentCnxn)])
  extends Data {

  def this() = this(None, Nil)

  override protected def generateLabelStr(): String = {
    s"cnxns($timestampLabel)"
  }
}
