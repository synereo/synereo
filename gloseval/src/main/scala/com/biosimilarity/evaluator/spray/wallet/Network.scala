package com.biosimilarity.evaluator.spray.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoinj.params.{MainNetParams, RegTestParams, TestNet3Params}

object Network {
  lazy val networkMode = ConfigFactory.load().getString("bitcoin.network.mode")
  lazy val params = networkMode match {
    case "MAIN" => MainNetParams.get()
    case "TEST" => TestNet3Params.get()
    case "REGTEST" => RegTestParams.get()
    case _ => RegTestParams.get()
  }
}
