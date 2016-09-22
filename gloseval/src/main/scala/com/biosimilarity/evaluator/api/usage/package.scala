package com.biosimilarity.evaluator.api

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.biosimilarity.evaluator.distribution.{EvalConfConfig => Config}
import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.Server
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.util._
import org.json4s.JsonAST.JArray
import org.json4s.jackson.JsonMethods._
import spray.can.Http

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future}
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

  private def printQueryResponses(hc: ActorRef, sessionUri: SessionUri)(implicit ec: ExecutionContext, timeout: Timeout): Future[Unit] =
    Future {
      new Pingerator(hc, sessionUri).foreach { (curr: JArray) =>
        val posts: List[String] = (curr \ "content" \ "pageOfPosts").extract[List[String]]
        println(s"""|
                    |>>>>>>>>>
                    |RESPONSE:
                    |>>>>>>>>>
                    |${pretty(render(curr))}""".stripMargin)
        if (posts.nonEmpty) {
          println(s"""|
                      |>>>>>>
                      |POSTS:
                      |>>>>>>""".stripMargin)
          posts.foreach { (s: String) =>
            println(pretty(render(parse(s))))
          }
        }
      }
    }

  def queryOnSelf(email: String, password: String, query: String): Unit = {
    val eventualQueryResponse: Future[Unit] = for {
      hc  <- eventualHostConnector(system)
      isr <- openSRPSession(hc, email, password)
      u   <- spawnSession(hc, isr.sessionURI)
      _   <- makeQueryOnSelf(hc, u, query)
      _   <- printQueryResponses(hc, u)
      _   <- hc.ask(Http.CloseAll)(timeout)
    } yield ()
    eventualQueryResponse.onComplete {
      case Success(_) =>
        println("""|
                   |>>>>
                   |DONE
                   |>>>>""".stripMargin)
      case Failure(ex: Throwable) =>
        logger.error(ex.getMessage)
    }
  }
}
