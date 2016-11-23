package com.synereo.wallet

import java.math.BigDecimal
import javax.xml.bind.DatatypeConverter

import foundation.omni.{CurrencyID, Ecosystem, OmniDivisibleValue, PropertyType}
import foundation.omni.tx.OmniTxBuilder
import com.synereo.wallet.config.RPCConfiguration
import com.synereo.wallet.models._
import org.bitcoinj.core._

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

trait OmniAPI extends RPCConfiguration {

  def getReceiveAddress(str: String): Address = Address.fromBase58(networkParams, str)

  def getAmpValue(amount: String) = OmniDivisibleValue.of(new BigDecimal(amount))

  def newECKey: ECKey = new ECKey()

  def restoreECKey(hexStr: String): ECKey = ECKey.fromPrivate(DatatypeConverter.parseHexBinary(hexStr))

  def getAddressByKeyHex(hex: String): Address = restoreECKey(hex).toAddress(networkParams)

  def getBalanceSummary(address: Address): BalanceSummary = {
    var errors: List[String] = Nil
    val amps = Try(omniClient.omniGetBalance(address, ampsID)) match {
      case Success(balanceEntry) => f"${balanceEntry.getBalance.numberValue().doubleValue()}%1.8f"
      case Failure(e) =>
        errors = e.getMessage :: errors
        "0.00"
    }
    val btcs = Try(omniClient.getBitcoinBalance(address)) match {
      case Success(coin) => coin.toPlainString
      case Failure(e) =>
        errors = e.getMessage :: errors
        "0.00"
    }

    BalanceSummary(address.toBase58, amps, btcs, errors)
  }

  def transfer(from: AMPKey, toAddress: Address, amount: String): String = {
    val fromAddress = from.address
    val omniAmount = getAmpValue(amount)
    val outputs = omniClient.listUnspentJ(fromAddress).toList
      .filter(p => p.getValue.value > 3000L)
    val builder = new OmniTxBuilder(networkParams)
    val tx = builder.createSignedSimpleSend(from.key, outputs, toAddress, ampsID, omniAmount)

    println(s"----> Sending $amount AMPs from ${fromAddress.toBase58} to ${toAddress.toBase58}")
    println(s"----> Transaction to send: \n${tx.toString}")

    omniClient.sendRawTransaction(tx).toString
  }

  def isConfirmed(hashStr: String) = {
    val tx = omniClient.send[java.util.LinkedHashMap[String, Any]]("gettransaction", hashStr)

    tx.get("confirmations") match {
      case c: java.lang.Integer => c >= MIN_NUM_OF_CONFIRMATIONS
      case _ => false
    }
  }

  def importAddress(addr: String): String =
    omniClient.send[String]("importaddress", addr, "", java.lang.Boolean.FALSE)

  def isAddressImported(addr: String) =
    omniClient.send[java.util.List[String]]("getaddressesbyaccount", "").contains(addr)

  def setGenerate(blocks: Long = 1L) = omniClient.setGenerate(true, blocks)

  def receiveBTC(address: Address, amount: String): String =
    omniClient.sendToAddress(address, Coin.parseCoin(amount)).toString

  def receiveAMP(senderAddress: Address, address: Address, amount: String) = omniClient.omniSend(senderAddress, address, ampsID, getAmpValue(amount)).toString

  def createTestCurrencyAndFundTheUser(userAddress: Address): Unit = {
    val fundingAddress = omniClient.getAccountAddress("")
    setGenerate(101L)
    receiveBTC(fundingAddress, "10")
    receiveBTC(userAddress, "10")
    setGenerate(6L)
    omniClient.omniSendIssuanceFixed(
      fundingAddress,
      Ecosystem.TOMNI,
      PropertyType.DIVISIBLE,
      new CurrencyID(CurrencyID.MIN_VALUE),
      "", "", propertyName, "", "Synereo Test Coin",
      getAmpValue("2000000000"))
    setGenerate(6L)
    receiveAMP(fundingAddress, userAddress, "10000")
    setGenerate(6L)
  }

}
