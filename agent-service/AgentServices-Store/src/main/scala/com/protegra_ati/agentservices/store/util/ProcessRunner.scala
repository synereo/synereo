package com.protegra_ati.agentservices.store.util

//import com.biosimilarity.evaluator.msgs.{RunProcessRequest, RunProcessResponse}
import java.io.File
import scala.collection.mutable
import scala.sys.process._

object ProcessRunner {
  import com.biosimilarity.evaluator.distribution._
  def run( request : ConcreteHL.RunProcessRequest ) : ConcreteHL.RunProcessResponse = {
    val cwd = request.workingDir.map( new File( _ ) )

    val p = Process( request.command, cwd, request.environmentVars : _* )

    val stdOutBuffer = new mutable.ListBuffer[String]
    val stdErrBuffer = new mutable.ListBuffer[String]

    val logger = ProcessLogger( stdOutBuffer += _, stdErrBuffer += _ )

    val exitCode = p ! logger

    ConcreteHL.RunProcessResponse( exitCode, stdOutBuffer.toList, stdErrBuffer.toList )
  }
}
