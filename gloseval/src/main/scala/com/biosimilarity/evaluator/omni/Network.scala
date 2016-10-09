package com.biosimilarity.evaluator.omni

import org.bitcoinj.params.{MainNetParams, RegTestParams, TestNet3Params}

object Network extends RPCConfiguration {
  lazy val params = networkMode match {
    case "MainNet" => MainNetParams.get()
    case "TestNet" => TestNet3Params.get()
    case "RegTest" => RegTestParams.get()
    case _ => RegTestParams.get()
  }
}
