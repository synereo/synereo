package com.biosimilarity.evaluator.omni

import com.biosimilarity.evaluator.spray.SessionManager
import org.bitcoinj.core.ECKey

case class AMPKey(identity: String, key: ECKey) {
  val address = key.toAddress(Network.params)
  val addressStr = address.toBase58
  val privHex = key.getPrivateKeyAsHex
  def findSessions = SessionManager.findSessionsByCap(identity)
}

object AMPKey extends Serializable {
  def apply(cap: String, keyHex: String): AMPKey =
    new AMPKey(cap, AMPUtilities.restoreECKey(keyHex))

  def apply(cap: String): AMPKey = {
    val result: AMPKey = new AMPKey(cap, AMPUtilities.newECKey)
    AMPUtilities.importAddress(result.addressStr)
    result
  }

}
