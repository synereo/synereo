package com.synereo.wallet.models

import java.math.BigDecimal

import com.synereo.wallet.OmniAPI
import com.synereo.wallet.config.RPCConfiguration
import org.bitcoinj.core.ECKey

case class AMPKey(identity: String, key: ECKey) extends Serializable with RPCConfiguration {
  val address = key.toAddress(AMPKey.networkParams)
  val addressStr = address.toBase58
  val privHex = key.getPrivateKeyAsHex

  def balance: BalanceSummary = AMPKey.getBalanceSummary(address)
  def transfer(to: AMPKey, amount: String): String = AMPKey.transfer(this, to.address, amount)
  def receiveBTC(amount: String): String = AMPKey.receiveBTC(address, amount)
  def receiveAMP(senderAddress: String, amount: String): String = AMPKey.receiveAMP(senderAddress, address, amount)
}

object AMPKey extends Serializable with OmniAPI {
  def apply(cap: String, keyHex: String): AMPKey =
    new AMPKey(cap, restoreECKey(keyHex))

  def apply(cap: String): AMPKey = {
    val instance = new AMPKey(cap, newECKey)
    importAddress(instance.addressStr)
    instance
  }
}
