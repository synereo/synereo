package com.biosimilarity.evaluator.msgs

case class RunProcessRequest(
  command : String,
  workingDir : Option[String],
  environmentVars : Seq[( String, String )]
)

case class RunProcessResponse(
  exitCode : Int,
  stdOut : Seq[String],
  stdErr : Seq[String]
)
