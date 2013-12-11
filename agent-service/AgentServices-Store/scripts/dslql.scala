// -*- mode: Scala;-*- 
// Filename:    dslql.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul  9 14:15:39 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

object DSLQuickStart {
  def begin() = {
    com.biosimilarity.evaluator.distribution.diesel.Server.run()
    com.biosimilarity.evaluator.distribution.bfactory.Server.run()

    import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext._
    import com.biosimilarity.evaluator.distribution.bfactory.BFactoryDefaultServiceContext.eServe._

    com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()
  }
}

DSLQuickStart.begin()

