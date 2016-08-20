package com.biosimilarity.evaluator.spray

import java.io.File

import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.util._
import org.slf4j.{Logger, LoggerFactory}

trait BootTasks {

  val logger: Logger = LoggerFactory.getLogger(classOf[BootTasks])

  def startServer(): Unit = {
    logger.info("Starting GLoSEval...")
    var service: Option[Server] = None
    sys.addShutdownHook {
      logger.info("Stopping GLoSEval...")
      val _: Option[Server] = service.map(_.stop())
      Thread.sleep(2000)
    }
    service = Some(Server().start())
  }

  def resetDatabase(): Unit = {
    logger.info("Resetting the database...")
    resetMongo()
  }

  def runImporter(file: Option[File]): Unit = {
    resetDatabase()
    startServer()
    val importerFile: File = file.getOrElse(Importer.serviceDemoDataFile())
    logger.info(s"Importing ${importerFile.getAbsolutePath}...")
    Thread.sleep(5000)
    Importer.fromFile(importerFile)
  }
}
