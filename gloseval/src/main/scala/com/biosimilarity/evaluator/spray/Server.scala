package com.biosimilarity.evaluator.spray

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.IO
import akka.pattern._
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.omni.AMPUtilities
import com.biosimilarity.evaluator.spray.SSLConfiguration._
import com.biosimilarity.evaluator.spray.srp.SRPSessionManager
import com.typesafe.config.{Config, ConfigFactory}
import spray.can.Http
import spray.can.server.ServerSettings

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

class Server(settings: ServerSettings, actorSystem: Option[ActorSystem] = None, listener: Option[ActorRef] = None)(
    implicit ec: ExecutionContext = ExecutionContext.global)
    extends Serializable {

  @transient
  implicit val timeout: Timeout = Timeout(5.seconds)

  // format: off
  def start(): Server = (actorSystem, listener) match {
    case (None, None) =>
      SessionManager.reset()
      SRPSessionManager.reset()
      CompletionMapper.reset()
      com.biosimilarity.evaluator.distribution.bfactory.BFactoryMapInitializer.makeMap()      
      if (EvalConfigWrapper.isOmniRequired() && !AMPUtilities.isHealthy) throw new Exception("Unable to connect to OmniCore")
      @transient
      val system: ActorSystem = ActorSystem("evaluator-system")
      @transient
      val listener: ActorRef = system.actorOf(Props[EvaluatorServiceActor], "evaluator-service")
      @transient
      val nonSSLSettings: ServerSettings = settings.copy(sslEncryption = false)
      IO(Http)(system) ! Http.Bind(listener = listener,
                                   interface = "0.0.0.0",
                                   port = EvalConfigWrapper.serverPort,
                                   settings = Some(nonSSLSettings))
      IO(Http)(system) ! Http.Bind(listener = listener,
                                   interface = "0.0.0.0",
                                   port = EvalConfigWrapper.serverSSLPort,
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

object Server extends Serializable {

  @transient
  private val serverConfig: Config = ConfigFactory.load()

  @transient
  val defaultSettings: ServerSettings = ServerSettings(serverConfig)

  def apply(settings: ServerSettings = defaultSettings): Server = new Server(settings)
}
