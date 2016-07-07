package com.biosimilarity.evaluator.spray

import akka.io.IO
import spray.can.Http
import akka.actor.{ ActorSystem, Props }
import com.biosimilarity.evaluator.omniRPC.OmniClient
import com.biosimilarity.evaluator.distribution.EvalConfConfig

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

  if (EvalConfConfig.isOmniRequired() && !OmniClient.canConnect()) throw new Exception("Unable to connect to OmniCore")

}
