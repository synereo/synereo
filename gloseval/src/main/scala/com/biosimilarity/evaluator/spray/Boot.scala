package com.biosimilarity.evaluator.spray

import java.io.File

import scopt.OptionParser

object Boot extends App with BootTasks {

  case class BootConfig(verbose: Boolean = false, mode: String = "", file: Option[File] = None)

  val parser: OptionParser[BootConfig] = new OptionParser[BootConfig]("gloseval") {

    override def showUsageOnError = true

    head("GLoSEval", "Alpha")

    help("help").text("prints this usage text\n")

    cmd("start")
      .optional()
      .action((_: Unit, c: BootConfig) => c.copy(mode = "start"))
      .text("  Start GLoSEval\n")

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
    case Some(BootConfig(_, mode, _)) if mode == "start" || mode == "" =>
      startServer()
    case Some(BootConfig(_, "import", file)) =>
      runImporter(file)
    case Some(BootConfig(_, "reset", file)) =>
      resetDatabase()
    case None =>
  }
}
