package com.biosimilarity.evaluator.spray

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.logging.{Level => JLevel, Logger => JLogger}

import com.biosimilarity.evaluator.distribution.{EvalConfConfig => Config}
import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.util._
import com.biosimilarity.lift.lib.amqp.AMQPUtil._
import com.biosimilarity.lift.model.store.mongo.MongoUtil._
import org.slf4j.{Logger, LoggerFactory}

import scala.sys.process._
import scala.util.{Failure, Success, Try}

trait BootTasks {

  sealed trait Certificate
  case object SelfSigned extends Certificate

  val logger: Logger = LoggerFactory.getLogger(classOf[BootTasks])

  def startServer(): Unit = {
    JLogger.getLogger("com.mongodb").setLevel(JLevel.OFF)
    val (mongoHost, mongoPort)   = (Config.readString("dbHost"), Config.readInt("dbPort"))
    val (rabbitHost, rabbitPort) = (Config.readString("DSLCommLinkServerHost"), Config.readInt("DSLCommLinkServerPort"))
    logger.info(s"Starting GLoSEval in ${Config.deploymentMode.toString.toLowerCase} mode...")
    (keystoreExists, Config.deploymentMode, rabbitIsRunning(rabbitHost, rabbitPort), mongoIsRunning(mongoHost, mongoPort)) match {
      case (false, _, _, _) =>
        logger.error("TLS Certificate not found.  Please run the 'gencert' command.")
        System.exit(1)
      case (true, Config.Distributed, false, _) =>
        logger.error(s"Could not connect to RabbitMQ instance at $rabbitHost:$rabbitPort")
        System.exit(1)
      case (true, _, _, false) =>
        logger.error(s"Could not connect to MongoDB instance at $mongoHost:$mongoPort")
        System.exit(1)
      case (true, _, _, true) =>
        var service: Option[Server] = None
        sys.addShutdownHook {
          logger.info("Stopping GLoSEval...")
          val _: Option[Server] = service.map(_.stop())
          Thread.sleep(2000)
        }
        service = Some(Server().start())
    }
  }

  def resetDatabase(): Unit = {
    logger.info("Resetting the database...")
    resetMongo()
  }

  def runImporter(file: Option[File]): Unit = {
    resetDatabase()
    startServer()
    val importerFile: File = file.getOrElse(Config.serviceDemoDataFile)
    logger.info(s"Importing ${importerFile.getAbsolutePath}...")
    Thread.sleep(5000)
    Importer.fromFile(importerFile)
  }

  def pwd: Path = Paths.get(".").toAbsolutePath.normalize()

  def resourcesDir: Path =
    Paths.get(classOf[Server].getProtectionDomain.getCodeSource.getLocation.toURI).getParent.getParent.resolve("resources")

  def keystoreExists: Boolean = Files.exists(resourcesDir.resolve("keystore.jks"))

  def clientCertExists: Boolean = Files.exists(resourcesDir.resolve("gloseval.pem"))

  def tryLogger(s: String): Try[Unit] = Try(logger.info(s))

  // format: off
  def createSelfSignedCertInKeystore(keystorePath: Path, storePass: String, keyPass: String): Try[Path] = {
    val keyStoreCommand: Seq[String] = Seq(
      "keytool", "-genkey", "-alias", "domain", "-keyalg", "RSA", "-validity", "365",
      "-keystore", keystorePath.toString, "-keypass", keyPass, "-storepass", storePass,
      "-dname", "CN=localhost, OU=Unknown, O=Unknown, L=Unknown, ST=Unknown, C=Unknown")
    Try(keyStoreCommand.!).map((_: Int) => keystorePath)
  }

  def createClientCertFromKeystore(keystorePath: Path, clientCertPath: Path, storePass: String): Try[Path] = {
    val clientCertCommand: Seq[String] = Seq(
      "keytool", "-exportcert", "-rfc", "-alias", "domain",
      "-file", clientCertPath.toString,
      "-keystore", keystorePath.toString, "-storepass", storePass)
    Try(clientCertCommand.!(ProcessLogger(line => ()))).map((_: Int) => clientCertPath)
  }
  // format: on

  def genSelfSignedCert(resourcesDir: Path, storePass: String, keyPass: String): Try[Unit] =
    for {
      _ <- tryLogger(s"""|Attempting to create a new keystore containing a self-signed certificate
                         |and a corresponding client certificate in $resourcesDir""".stripMargin.replace("\n", " "))
      d <- Try(Files.createDirectories(resourcesDir))
      _ <- tryLogger(s"Directory created/exists at $d")
      k <- createSelfSignedCertInKeystore(d.resolve("keystore.jks"), storePass, keyPass)
      _ <- tryLogger(s"Keystore containing a self-signed certificate created at $k")
      c <- createClientCertFromKeystore(k, d.resolve("gloseval.pem"), storePass)
      _ <- tryLogger(s"Client certificate created at $c")
    } yield ()

  def genCert(certificate: Certificate): Unit = {
    logger.info(s"Current working directory is $pwd")
    (certificate, keystoreExists, clientCertExists) match {
      case (SelfSigned, false, false) =>
        val keyPass: String   = Config.readString("keypass")
        val storePass: String = Config.readString("storepass")
        genSelfSignedCert(resourcesDir, storePass, keyPass) match {
          case Success(_) =>
            logger.info("We are good to go!")
          case Failure(ex) =>
            logger.error(ex.getMessage)
            logger.error("Unfortunately, we were unable to finish this process.")
            logger.error(s"""Please remove the "${pwd.relativize(resourcesDir)}" directory and try again.""")
            System.exit(1)
        }
      case (_, true, true) =>
        logger.info(s"""Keystore and client certificate already exist in "${pwd.relativize(resourcesDir)}".""")
        logger.info("We should be good to go.")
      case (_, true, false) =>
        logger.error("Keystore already exists.")
        logger.error(s"""Please remove the "${pwd.relativize(resourcesDir)}" directory and try again.""")
      case (_, false, true) =>
        logger.error("Client certificate already exists.")
        logger.error(s"""Please remove the "${pwd.relativize(resourcesDir)}" directory and try again.""")
    }
  }
}
