package com.synereo.wallet.config

import java.net.URI

import com.msgilligan.bitcoinj.rpc.RPCConfig
import com.typesafe.config.ConfigFactory
import foundation.omni.CurrencyID
import foundation.omni.rpc.OmniClient
import org.bitcoinj.core.NetworkParameters
import org.bitcoinj.params.{MainNetParams, RegTestParams, TestNet3Params}
import org.slf4j.LoggerFactory

import scala.util.Try

trait RPCConfiguration {
  val config       = ConfigFactory.load
  val logger       = LoggerFactory.getLogger("wallet")

  val networkMode  = config.getString("bitcoin.network.mode")
  val networkParams: NetworkParameters = networkMode match {
    case "MainNet" => MainNetParams.get()
    case "TestNet" => TestNet3Params.get()
    case "RegTest" => RegTestParams.get()
    case _ => RegTestParams.get()
  }

  val rpcUser      = config.getString("rpc.user")
  val rpcPassword  = config.getString("rpc.password")
  val rpcURL       = config.getString("rpc.url")
  val rpcConfig    = new RPCConfig(networkParams, new URI(rpcURL), rpcUser, rpcPassword)
  val omniClient   = new OmniClient(rpcConfig)

  val omniWalletAddress  = config.getString("omni.wallet.address")

  val propertyID         = config.getString("omni.property.id").toLong
  val ampsID: CurrencyID = new CurrencyID(propertyID)

  val MIN_NUM_OF_CONFIRMATIONS: Int = 1

  def isHealthy = Try(omniClient.getInfo).isSuccess
}
