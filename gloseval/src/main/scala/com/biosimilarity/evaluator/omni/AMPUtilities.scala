package com.biosimilarity.evaluator.omni

import com.biosimilarity.evaluator.spray.srp.ConversionUtils
import foundation.omni.OmniDivisibleValue
import org.bitcoinj.core._
import ConversionUtils._
import com.google.common.util.concurrent.{FutureCallback, ListenableFuture}
import foundation.omni.tx.OmniTxBuilder

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._
import com.google.common.util.concurrent.Futures

object AMPUtilities extends RPCConfiguration {

  def getReceiveAddress(str: String): Address = Address.fromBase58(Network.params, str)

  def getAmpValue(amount: BigDecimal) = OmniDivisibleValue.of(amount.bigDecimal)

  def newKeyHex: String = {
    val key = new ECKey()
    println("New key created")
    println(key.toString)
    toHex(key.getPrivKey)
  }

  def restoreECKey(hexStr: String): ECKey = ECKey.fromPrivate(fromHex(hexStr))

  def getAddressByKeyHex(hex: String): Address = restoreECKey(hex).toAddress(Network.params)

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

    val future: ListenableFuture[TransactionConfidence] = tx.getConfidence.getDepthFuture(1)
    val callback: FutureCallback[TransactionConfidence] = new FutureCallback[TransactionConfidence] {
      override def onFailure(t: Throwable): Unit = t.printStackTrace()

      override def onSuccess(result: TransactionConfidence): Unit = {
        println(s"Sending balance changes by transaction ${toHexString(result.getTransactionHash)}")
        val b1 = getBalanceSummary(from.address)
        from.findSessions.foreach(s => b1.cometMessage(s))
        val b2 = getBalanceSummary(to.address)
        to.findSessions.foreach(s => b2.cometMessage(s))
      }
    }

    Futures.addCallback(future, callback)

    println(s"----> Sending $amount AMPs from ${fromAddress.toBase58} to $to")
    println(s"----> Transaction to send: \n${tx.toString}")

    toHexString(omniClient.sendRawTransaction(tx))
  }

  def toHexString(hash: Sha256Hash): String = Utils.HEX.encode(hash.getBytes)

}
