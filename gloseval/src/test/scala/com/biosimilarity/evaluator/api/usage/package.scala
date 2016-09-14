package com.biosimilarity.evaluator.api

import java.io.File

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.{EvalConfConfig => Config}
import com.biosimilarity.evaluator.importer._
import com.biosimilarity.evaluator.spray.Server
import com.biosimilarity.evaluator.spray.util._
import org.json4s.JsonAST.JArray
import org.json4s.jackson.JsonMethods._
import spray.can.Http

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Success}

package object usage extends ApiClient {

  val system: ActorSystem = ActorSystem()

  implicit val timeout: Timeout = Timeout(FiniteDuration(15, SECONDS))
  implicit val ec               = system.dispatcher

  private var serverInstance: Option[Server] = None

  def resetDatabase(): Unit = {
    logger.info("Resetting the database...")
    resetMongo()
  }

  def loadQueryData(dataJsonFileName: String): Int = {
    loadQueryData(new File(dataJsonFileName))
  }

  def loadQueryData(dataJsonFile: File = Config.serviceDemoDataFile): Int = {
    stopServer()
    resetDatabase()
    startServer()
    Importer.fromFile(dataJsonFile)
  }

  def startServer(): Unit = {
    logger.info(s"Starting GLoSEval in ${Config.deploymentMode.toString.toLowerCase} mode...")
    serverInstance = Some(Server().start())
    Thread.sleep(3000)
  }

  def stopServer(): Unit = {
    logger.info("Stopping GLoSEval...")
    serverInstance.map(_.stop())
    serverInstance = None
  }

  def query(email: String, password: String, query: String): Unit = {
    val eventualQueryResponse: Future[JArray] = for {
      hc <- eventualHostConnector(system)
      s  <- openSRPSession(hc, email, password)
      u  <- spawnSession(hc, s)
      _  <- makeQueryOnSelf(hc, u, query)
      q  <- pingUntilPong(hc, u)
      _  <- hc.ask(Http.CloseAll)(timeout)
    } yield q
    eventualQueryResponse.onComplete {
      case Success(resp) =>
        println(s"""|>>>>>>>>>
                    |RESPONSE:
                    |>>>>>>>>>
                    |${pretty(render(resp))}""".stripMargin)
      case Failure(ex: Throwable) =>
        logger.error(ex.getMessage)
    }
  }
}
