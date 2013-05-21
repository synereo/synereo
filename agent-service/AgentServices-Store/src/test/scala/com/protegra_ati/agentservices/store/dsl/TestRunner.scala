package com.protegra_ati.agentservices.store.dsl

import com.protegra_ati.agentservices.store.dsl.protocols.ProtocolMessageTest

object TestRunner {
  def run() {
    specs2.run(new ProtocolMessageTest)
  }
}

// com.protegra_ati.agentservices.store.dsl.TestRunner.run()