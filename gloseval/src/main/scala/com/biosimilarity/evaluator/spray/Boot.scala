package com.biosimilarity.evaluator.spray

import com.typesafe.config.{Config, ConfigFactory}
import spray.can.server.ServerSettings

import scala.sys.ShutdownHookThread

object Boot extends App {

  // TODO: Remove sleep below once race condition is fixed
  // @@GS - is it fixed??
  // Thread.sleep(3000)

  val config: Config = ConfigFactory.load()

  val settings: ServerSettings = ServerSettings(config)

  val sht: ShutdownHookThread = sys.addShutdownHook(shutdown())

  val service: Server = new Server(settings).start()

  private def shutdown(): Unit = { val _ = service.stop(); () }
}
