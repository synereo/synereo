package com.biosimilarity.evaluator.spray

import akka.io.IO
import spray.can.Http
import akka.actor.{ActorSystem, Props}

object Boot extends App
  with Serializable {

  //TODO: Remove sleep below once race condition is fixed
  // @@GS - is it fixed??
  Thread.sleep(3000)

  com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()

  @transient
  implicit val system = ActorSystem("evaluator-system")

  @transient
  val service = system.actorOf(Props[EvaluatorServiceActor], "evaluator-service")   

  IO(Http) ! Http.Bind(listener = service, interface = "0.0.0.0", port = 9876)
}
