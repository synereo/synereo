package com.biosimilarity.evaluator.omni

import com.biosimilarity.evaluator.spray.SessionManager
import org.bitcoinj.core.ECKey

case class AMPKey(identity: String, key: ECKey) {
  val address = key.toAddress(Network.params)
  val addressStr = address.toBase58
  def findSessions = SessionManager.findSessionsByCap(identity)
}

object AMPKey extends Serializable {
  def apply(cap: String, keyHex: String): AMPKey =
    new AMPKey(cap, AMPUtilities.restoreECKey(keyHex))
}
