package com.biosimilarity.evaluator.spray

import java.io.File

import scopt.OptionParser

object Boot extends App with BootTasks {

  case class BootConfig(verbose: Boolean = false, mode: String = "", file: Option[File] = None, certificate: Option[Certificate] = None, version: Boolean = false)

  val parser: OptionParser[BootConfig] = new OptionParser[BootConfig]("gloseval") {

    override def showUsageOnError = true

    head("GLoSEval", "Alpha")

    help("help").text("prints this usage text\n")

    opt[Unit]('v', "version").action( (_, c) => c.copy(version = true) ).text("Get version information.")

    cmd("start")
      .optional()
      .action((_: Unit, c: BootConfig) => c.copy(mode = "start"))
      .text("  Start GLoSEval\n")

    cmd("gencert")
      .action((_: Unit, c: BootConfig) => c.copy(mode = "gencert"))
      .text("  Generate a TLS Certificate")
      .children(opt[Unit]("self-signed").action((x, c) => c.copy(certificate = Some(SelfSigned))).text("Generate a self-signed certificate.\n"))

    cmd("import")
      .action((_: Unit, c: BootConfig) => c.copy(mode = "import"))
      .text("  Reset the Database, Start GLoSEval, and run the Importer")
      .children(opt[File]('f', "file").optional().valueName("<file>").action((x, c) => c.copy(file = Some(x))).text("file to import.\n"))

    note("""  If no file is specified, the importer will import the file specified
           |  in the 'ImporterServiceDemoDataFile' field of eval.conf.
           |""".stripMargin)

    cmd("reset")
      .action((_: Unit, c: BootConfig) => c.copy(mode = "reset"))
      .text("  Reset the Database")
  }

  parser.parse(args, BootConfig()) match {
    case Some(BootConfig(_, _, _, _, true)) =>
      version()
    case Some(BootConfig(_, mode, _, _, false)) if mode == "start" || mode == "" =>
      startServer()
    case Some(BootConfig(_, "gencert", _, Some(cert), false)) =>
      genCert(cert)
    case Some(BootConfig(_, "import", file, _, false)) =>
      runImporter(file)
    case Some(BootConfig(_, "reset", file, _, false)) =>
      resetDatabase()
    case None =>
  }
}
