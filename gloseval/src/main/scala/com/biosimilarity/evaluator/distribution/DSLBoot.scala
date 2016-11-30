package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer

object DSLBoot {
  def start(makeMap: Boolean = true) = {
    diesel.Server.run()
    bfactory.Server.run()
    if (makeMap) {
      BFactoryMapInitializer.makeMap()
    }

    ConnectionManager.pureConvenienceMethod()
  }
}
