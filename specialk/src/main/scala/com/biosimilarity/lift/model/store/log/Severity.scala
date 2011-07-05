// -*- mode: Scala;-*- 
// Filename:    Severity.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jun 29 14:56:47 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.monitor

import com.biosimilarity.lift.model.ApplicationDefaults

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList

import org.prolog4j._

//import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

object Severity extends Enumeration()
{
  type Severity = Value
  val Fatal, Error, Warning, Info, Debug, Trace = Value
}

trait SeverityOps {
  def SeverityFromOption( level : Option[ String ] ) : Severity.Value =
  {
    level match {
      case Some( x ) => {
        SeverityFromString( x )
      }
      case None => {
        Severity.Debug
      }
    }
  }

  def SeverityFromString( level : String ) : Severity.Value =
  {
    level.toLowerCase() match {
      case "fatal" => {
        Severity.Fatal
      }
      case "error" => {
        Severity.Error
      }
      case "warning" => {
        Severity.Warning
      }
      case "info" => {
        Severity.Info
      }
      case "debug" => {
        Severity.Debug
      }
      case "trace" => {
        Severity.Trace
      }
      case _ => {
        Severity.Debug
      }
    }
  }
}

trait LoggingSeverityClasses {
  trait LoggingSeverityClass {
    def level : Int
    def fact : java.io.Serializable
  }

  trait LoggingSeverityExceptionAccess {
    def exception : Option[Exception]
  }

  case class LoggingSeverityFatal(
    override val level : Int,
    override val fact : java.io.Serializable,
    override val exception : Option[Exception]
  ) extends LoggingSeverityClass
       with LoggingSeverityExceptionAccess

  case class LoggingSeverityError(
    override val level : Int,
    override val fact : java.io.Serializable,
    override val exception : Option[Exception]
  ) extends LoggingSeverityClass
       with LoggingSeverityExceptionAccess

  case class LoggingSeverityWarning(
    override val level : Int,
    override val fact : java.io.Serializable
  ) extends LoggingSeverityClass

  case class LoggingSeverityInfo(
    override val level : Int,
    override val fact : java.io.Serializable
  ) extends LoggingSeverityClass

  case class LoggingSeverityDebug(
    override val level : Int,
    override val fact : java.io.Serializable,
    override val exception : Option[Exception]
  ) extends LoggingSeverityClass
       with LoggingSeverityExceptionAccess

  case class LoggingSeverityTrace(
    override val level : Int,
    override val fact : java.io.Serializable,
    override val exception : Option[Exception]
  ) extends LoggingSeverityClass
       with LoggingSeverityExceptionAccess

  def protoLoggingSeverityFatal : LoggingSeverityFatal
  lazy val theProtoLoggingSeverityFatal : LoggingSeverityFatal =
    protoLoggingSeverityFatal
  
  def protoLoggingSeverityError : LoggingSeverityError
  lazy val theProtoLoggingSeverityError : LoggingSeverityError =
    protoLoggingSeverityError

  def protoLoggingSeverityWarning : LoggingSeverityWarning
  lazy val theProtoLoggingSeverityWarning : LoggingSeverityWarning =
    protoLoggingSeverityWarning

  def protoLoggingSeverityInfo : LoggingSeverityInfo
  lazy val theProtoLoggingSeverityInfo : LoggingSeverityInfo =
    protoLoggingSeverityInfo

  def protoLoggingSeverityDebug : LoggingSeverityDebug
  lazy val theProtoLoggingSeverityDebug : LoggingSeverityDebug =
    protoLoggingSeverityDebug

  def protoLoggingSeverityTrace : LoggingSeverityTrace
  lazy val theProtoLoggingSeverityTrace : LoggingSeverityTrace =
    protoLoggingSeverityTrace
}

trait LoggingSeverityScope {
  type LSClassTypes <: LoggingSeverityClasses
  def protoLSClasses : LSClassTypes
  val LSClasses : LSClassTypes = protoLSClasses
}
