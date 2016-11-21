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

import scala.collection.JavaConversions._

trait RPCConfiguration {
  val config       = ConfigFactory.load(ConfigFactory.parseFile(new java.io.File("eval.conf")))
  val logger       = LoggerFactory.getLogger("wallet")

  val networkMode  = config.getString("omni.networkMode")
  val networkParams: NetworkParameters = networkMode match {
    case "MainNet" => MainNetParams.get()
    case "TestNet" => TestNet3Params.get()
    case "RegTest" => RegTestParams.get()
    case _ => RegTestParams.get()
  }

  val isRegTestMode = networkMode.equals("RegTest")

  val rpcUser      = config.getString("omni.rpcUser")
  val rpcPassword  = config.getString("omni.rpcPassword")
  val rpcURL       = config.getString("omni.rpcUrl")
  val rpcConfig    = new RPCConfig(networkParams, new URI(rpcURL), rpcUser, rpcPassword)
  val omniClient   = new OmniClient(rpcConfig)

  val propertyName = "SynereoCoin"
  val propertyId = config.getString("property.id")

  val MIN_NUM_OF_CONFIRMATIONS: Int = 1

  def isHealthy = Try(omniClient.getInfo).isSuccess

  lazy val ampsID: CurrencyID = if(isHealthy) {
    networkMode match {
      case "MainNet" => CurrencyID.AMP
      case "TestNet" => new CurrencyID(propertyId.toLong)
      case _ =>
        val properties = omniClient.omniListProperties().toList.filter(p => p.getName.equals(propertyName))
        if(properties.nonEmpty) properties.head.getPropertyid
        else throw new Exception("Synereo Coin property is missing")
    }
  } else throw new Exception("Unable to connect to OmniCore")
}
