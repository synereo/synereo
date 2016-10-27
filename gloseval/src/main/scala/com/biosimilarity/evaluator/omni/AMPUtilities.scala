package com.biosimilarity.evaluator.omni

import javax.xml.bind.DatatypeConverter
import foundation.omni.OmniDivisibleValue
import org.bitcoinj.core._
import com.biosimilarity.evaluator.spray.SessionManager
import foundation.omni.tx.OmniTxBuilder

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._

object AMPUtilities extends RPCConfiguration {

  val MIN_NUM_OF_CONFIRMATIONS: Int = 1

  def getAmpValue(amount: BigDecimal) = OmniDivisibleValue.of(amount.bigDecimal)

  def newECKey: ECKey = new ECKey()

  def restoreECKey(hexStr: String): ECKey = ECKey.fromPrivate(DatatypeConverter.parseHexBinary(hexStr))

  def getBalanceSummary(address: Address): BalanceSummary = {
    var errors: List[String] = Nil
    val amps = Try(omniClient.omniGetBalance(address, ampsID)) match {
      case Success(balanceEntry) => balanceEntry.getBalance.numberValue().toString
      case Failure(e) =>
        errors = e.getMessage :: errors
        "0"
    }
    var sumBTC: Long = 0L
    omniClient.listUnspentJ(address).toList.foreach(p => sumBTC += p.getValue.value)

    BalanceSummary(address.toBase58, amps, Coin.valueOf(sumBTC).toPlainString, errors)
  }

  def transfer(from: AMPKey, to: AMPKey, amount: BigDecimal) = {
    val fromAddress = from.address
    val toAddress = to.address
    val omniAmount = getAmpValue(amount.bigDecimal)
    val outputs = omniClient.listUnspentJ(fromAddress).toList
      .filter(p => p.getValue.value > 3000L)
    val builder = new OmniTxBuilder(Network.params)
    val tx = builder.createSignedSimpleSend(from.key, outputs, toAddress, ampsID, omniAmount)

    println(s"----> Sending $amount AMPs from ${fromAddress.toBase58} to $to")
    println(s"----> Transaction to send: \n${tx.toString}")

    val hash: Sha256Hash = omniClient.sendRawTransaction(tx)

    SessionManager.addMonitoredTransaction(from.identity, MonitoredTransaction(hash, from, to))

    hash.toString
  }

  def isConfirmed(hash: Sha256Hash) = omniClient.getTransaction(hash).getConfirmations >= MIN_NUM_OF_CONFIRMATIONS

  def importAddress(addr: String): String = omniClient.send[String]("importaddress", addr, "", java.lang.Boolean.FALSE)

  def isAddressImported(addr: String) = omniClient.send[java.util.List[String]]("getaddressesbyaccount", "").contains(addr)

}
