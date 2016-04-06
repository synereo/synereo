package com.biosimilarity.evaluator.spray

//@@GS is this used ??
import com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer

//import spray.can.server.SprayCanHttpServerApp
//import spray.http._
import akka.io.IO
import spray.can.Http
//import spray.client.pipelining._

import akka.actor.{ActorSystem, Props}


object Boot extends App
 //with SprayCanHttpServerApp
 with Serializable {

  //TODO: Remove sleep below once race condition is fixed
  // @@GS - is it fixed??
  Thread.sleep(3000)

   com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()

   @transient
   implicit val system = ActorSystem("evaluator-system")

   // create and start our service actor
   @transient
   val service = system.actorOf(Props[EvaluatorServiceActor], "evaluator-service")   

   // create a new HttpServer using our handler tell it where to bind to
   //newHttpServer(service) ! Bind(interface = "0.0.0.0", port = 9876)


   IO(Http) ! Http.Bind(listener = service, interface = "0.0.0.0", port = 9876)

}
