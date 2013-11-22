package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.ConcreteHL
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

trait ProtocolMessage {
  val sessionId: Option[String]
  protected val innerLabel = s"sessionId(${generateLabelValue(sessionId)})"
  private lazy val label = ("protocolMessage(" + getClass.getName.trimPackage.toCamelCase + "(" + innerLabel + "))").toLabel

  protected def generateLabelValue(value: Option[String]): String = {
    value match {
      case Some(v) => "\"" + v + "\""
      case None => "_"
    }
  }

  def toCnxnCtxtLabel: CnxnCtxtLabel[String, String, String] = {
    label
  }

  def toGround: mTT.Ground = {
    mTT.Ground(ConcreteHL.PostedExpr(this))
  }
}
