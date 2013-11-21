// -*- mode: Scala;-*- 
// Filename:    BFactoryDefaultService.scala 
// Authors:     lgm                                                    
// Creation:    Thu Nov 21 10:32:00 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import java.net.URI
import java.util.Date
import java.util.UUID

package bfactory {
  object BFactoryDefaultServiceContext
  extends Serializable 
  with UseCaseHelper {    
    @transient
    lazy val eServe =
      new BFactoryCommsService
         with EvalConfig
         with BFactoryCommLinkConfiguration
         with Serializable {
         }    
  }  
}
