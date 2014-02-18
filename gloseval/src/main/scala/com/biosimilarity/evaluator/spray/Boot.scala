package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer
import spray.can.server.SprayCanHttpServerApp
import akka.actor.Props


object Boot extends App
 with SprayCanHttpServerApp
 with Serializable {

  BFactoryMapInitializer.makeMap()

  //TODO: Remove sleep below once race condition is fixed
  Thread.sleep(3000)

   // create and start our service actor
   @transient
   val service = system.actorOf(Props[EvaluatorServiceActor], "evaluator-service")   

   // create a new HttpServer using our handler tell it where to bind to
   newHttpServer(service) ! Bind(interface = "0.0.0.0", port = 9876)
}
