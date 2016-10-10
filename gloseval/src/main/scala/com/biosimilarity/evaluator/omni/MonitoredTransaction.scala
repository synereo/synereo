package com.biosimilarity.evaluator.omni

import org.bitcoinj.core.Sha256Hash

case class MonitoredTransaction(hash: Sha256Hash, from: AMPKey, to: AMPKey) {

  val id: String = AMPUtilities.toHexString(hash)

}
