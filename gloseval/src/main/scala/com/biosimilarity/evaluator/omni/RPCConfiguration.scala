package com.biosimilarity.evaluator.omni

import java.net.URI

import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.msgilligan.bitcoinj.rpc.RPCConfig
import foundation.omni.CurrencyID
import foundation.omni.rpc.OmniClient

import scala.util.Try

trait RPCConfiguration {
  val networkMode = EvalConfigWrapper.readStringOrElse("BitcoinNetworkMode", "TestNet")
  val RPC_USER    = EvalConfigWrapper.readStringOrElse("OmniRPCUser", "user")
  val RPC_PWD     = EvalConfigWrapper.readStringOrElse("OmniRPCPass", "pass")
  val OMNI_URI    = EvalConfigWrapper.readStringOrElse("OmniRPCURI", "http://localhost")
  val AMP_PROP_ID = EvalConfigWrapper.readStringOrElse("AMPPropertyID", "0").toLong
  val config      = new RPCConfig(Network.params, new URI(OMNI_URI), RPC_USER, RPC_PWD)
  val omniClient  = new OmniClient(config)
  val ampsID: CurrencyID = new CurrencyID(AMP_PROP_ID)

  def isHealthy = Try(omniClient.getInfo).isSuccess
}
