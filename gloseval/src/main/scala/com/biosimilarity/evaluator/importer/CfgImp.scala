// -*- mode: Scala;-*- 
// Filename:    CfgImp.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jan 19 16:28:07 2016 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.importer

import com.biosimilarity.evaluator.distribution.{AccordionConfiguration, DSLCommLinkConfiguration, EvalConfig, EvaluationCommsService}
import com.biosimilarity.evaluator.importer.dtos._
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.importer.utils.mailinator.Mailinator
import com.biosimilarity.evaluator.spray.{BTCHandler, DownStreamHttpCommsT, EvalHandler}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.util.Random

import scalaj.http.Http

trait ImporterConfig {
  self : EvalConfig =>
    var _serviceHost : Option[String] = None 
    var _servicePort : Option[Int] = None
    //var _serviceEmailSenderAddress : Option[String] = None
    //var _serviceMailinatorHost : Option[String] = None
    //var _serviceMailinatorKey : Option[String] = None
    //var _serviceUserChunkSize : Option[String] = None
    var _serviceDemoDataFile : Option[String] = None
    var _mongodbPath : Option[String] = None
    //var _serviceSystemLabelsFile : Option[String] = None

    def serviceHost() = {
      _serviceHost match {
        case Some( sh ) => sh
        case None => {
          val sh = evalConfig().getString( "ImporterServiceHost" )
          _serviceHost = Some( sh )
          sh
        }
      }      
    }
    def servicePort() = {
      _servicePort match {
        case Some( sp ) => sp
        case None => {
          val sp = evalConfig().getInt( "ImporterServicePort" )
          _servicePort = Some( sp )
          sp
        }
      }      
    }

    //private var GLOSEVAL_HOST = "http://52.35.39.85:9876/api"
    def serviceHostURI() = {
      s"http://${serviceHost}:${servicePort}/api"
    }

  //private val GLOSEVAL_SENDER = "splicious.ftw@gmail.com"
    /*
    def serviceEmailSenderAddress() = {
      _serviceEmailSenderAddress match {
        case Some( semsa ) => semsa
        case None => {
          val semsa = evalConfig().getString( "ImporterServiceEmailSenderAddress" )
          _serviceEmailSenderAddress = Some( semsa )
          semsa
        }
      }
    }

  def serviceMailinatorHost() = {
      _serviceMailinatorHost match {
        case Some( smh ) => smh
        case None => {
          val smh = evalConfig().getString( "ImporterServiceMailinatorHost" )
          _serviceMailinatorHost = Some( smh )
          smh
        }
      }
    }

  // private val MAILINATOR_KEY = "efa3a1b773db4f0c9492686d24bed415"
    def serviceMailinatorKey() = {
      _serviceMailinatorKey match {
        case Some( smk ) => smk
        case None => {
          val smk = evalConfig().getString( "ImporterServiceMailinatorKey" )
          _serviceMailinatorKey = Some( smk )
          smk
        }
      }
    }

    def serviceUserChunkSize() = {
      _serviceUserChunkSize match {
        case Some( sucs ) => sucs
        case None => {
          val sucs = evalConfig().getString( "ImporterServiceUserChunkSize" ) 
          _serviceUserChunkSize = Some( sucs )
          sucs
        }
      }
    }
*/

  def serviceDemoDataFile() = {
    _serviceDemoDataFile match {
      case Some( sddf ) => sddf
      case None => {
        evalConfig().getString( "ImporterServiceDemoDataFile" )
      }
    }
  }

  def mongodbPath() = {
    _mongodbPath match {
      case Some( sddf ) => sddf
      case None => {
        evalConfig().getString( "MongodbPath" )
      }
    }
  }

  /*
    def serviceSystemLabelsFile() = {
      _serviceSystemLabelsFile match {
        case Some( sslf ) => sslf
        case None => {
          val sslf = evalConfig().getString( "ImporterServiceSystemLabelsFile" ) 
          _serviceUserChunkSize = Some( sslf )
          sslf
        }
      }
    }
    */
}

