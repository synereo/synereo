// -*- mode: Scala;-*- 
// Filename:    RunProcess.scala 
// Authors:     lgm                                                    
// Creation:    Fri Dec 13 16:33:51 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.dsl

trait SystemManagement {
  self : AbstractHL =>
  trait SystemRequest
  trait SystemResponse

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
}
