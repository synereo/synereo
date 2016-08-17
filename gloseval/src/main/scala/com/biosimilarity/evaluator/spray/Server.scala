package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.IO
import akka.pattern._
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfConfig
import com.biosimilarity.evaluator.omni.OmniClient
import com.biosimilarity.evaluator.spray.SSLConfiguration._
import spray.can.Http
import spray.can.server.ServerSettings

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

class Server(settings: ServerSettings, actorSystem: Option[ActorSystem] = None, listener: Option[ActorRef] = None)(
    implicit ec: ExecutionContext = ExecutionContext.global) {

  implicit val timeout: Timeout = Timeout(5.seconds)

  // format: off
  def start(): Server = (actorSystem, listener) match {
    case (None, None) =>
      com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()
      if (EvalConfConfig.isOmniRequired() && !OmniClient.canConnect()) throw new Exception("Unable to connect to OmniCore")
      val system: ActorSystem            = ActorSystem("evaluator-system")
      val listener: ActorRef             = system.actorOf(Props[EvaluatorServiceActor], "evaluator-service")
      val nonSSLSettings: ServerSettings = settings.copy(sslEncryption = false)
      IO(Http)(system) ! Http.Bind(listener = listener,
                                   interface = "0.0.0.0",
                                   port = EvalConfConfig.serverPort,
                                   settings = Some(nonSSLSettings))
      IO(Http)(system) ! Http.Bind(listener = listener,
                                   interface = "0.0.0.0",
                                   port = EvalConfConfig.serverSSLPort,
                                   settings = Some(settings))(sslEngineProvider)
      new Server(settings, Some(system), Some(listener))(system.dispatcher)
    case (Some(_), Some(_)) =>
      this
    case (_, _) =>
      throw new IllegalStateException("Server has reached an inconsistent state")
  }
  // format: on

  def stop(): Server = (actorSystem, listener) match {
    case (None, None) =>
      this
    case (Some(as), Some(la)) =>
      IO(Http)(as).ask(Http.CloseAll).onComplete { (_: Try[Any]) =>
        as.stop(IO(Http)(as))
        as.stop(la)
        as.shutdown()
      }
      new Server(settings)
    case (_, _) =>
      throw new IllegalStateException("Server has reached an inconsistent state")
  }
}
