package com.protegra_ati.agentservices.runner

object Diesel {
  def main(args: Array[String]) {
    com.biosimilarity.evaluator.distribution.diesel.Server.run()
    com.biosimilarity.evaluator.distribution.bfactory.Server.run()

    import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext._
    import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext.eServe._

    com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()
  }
}
