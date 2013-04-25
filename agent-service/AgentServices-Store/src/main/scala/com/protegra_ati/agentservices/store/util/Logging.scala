// -*- mode: Scala;-*- 
// Filename:    Slog.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 11:17:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store.util

import com.typesafe.config._

import scala.collection.mutable.HashMap

import org.apache.log4j.{PropertyConfigurator, Level, Logger}
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

object Severity extends Enumeration()
{
  type Severity = Value
  val Fatal, Error, Warning, Info, Debug, Trace = Value
}
//logging/tracing too expensive to leave on by default
object LogConfiguration {
  def SeverityFromString(level: String): Severity.Value =
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
        Severity.Fatal
      }
    }
  }

  lazy val config = {
    ConfigFactory.load(ConfigFactory.parseFile(new File("log_agentservices.conf")))
  }
  var traceLevel =
    try {
      SeverityFromString( config.getString( "traceLevel" ) )
    }
    catch {
      case e => Severity.Trace
    }
  var logLevel =
    try {
      SeverityFromString( config.getString( "logLevel" ) )
    }
    catch {
      case e => Severity.Trace
    }

  lazy val logger = {
    PropertyConfigurator.configure( "log_agentservices.properties" )
    Logger.getLogger( this.getClass.getName )
  }
}

object Reporting
{
  @transient
  lazy val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SSS")
}

trait Reporting
{
  import LogConfiguration._

  def prettyPrintElisions() : HashMap[String,String] =
    {
      val map = new HashMap[String,String]()
      //map += ( "{" -> "" )
      //map += ( "}" -> "" )
      map += ( "&amp;" -> "&" )
      map += ( "vamp;" -> "&" )
      map += ("amp;" -> "&" )
      map += ( "&quot;" -> "\"" )
      map += ( "vquot;" -> "\"" )
      map += ( "quot;" -> "\"" )
      map += ( "_-" -> "" )
      //map += ( ":" -> "" )
      map += ( "@class" -> "" )
      map += ( "com.biosimilarity.lift." -> "kvdb:" )
      map += ( "com.protegra_ati.agentservices.store." -> "agent-services:" )
      map += ( "MonadicTermTypes" -> "" )
      map += ( "AgentTS$TheMTT$" -> "" )
      map += ( "Groundvstring,$" -> "" )
      map += ( "Groundstring,$" -> "" )
      map += ( ",outer" -> "" )
      map += ( "&lt;" -> "<" )
      map += ( "&gt;" -> ">" )
      map
    }       

  def prettyPrint(value: String): String =
  {
    ( value /: prettyPrintElisions )(
      { ( acc, e ) => { acc.replace( e._1, e._2 ) } }
    ) 
  }

  def header(level: Severity.Value): String =
  {
    "[" + Reporting.dateFormat.format(Calendar.getInstance().getTime()) + "] =" + level.toString.toUpperCase + " REPORT==== Thread " + Thread.currentThread.getName + " ==="
  }

  def wrap[ A ](fact: A): String =
  {
    //keep it readable on println but still send it all to the log
    val value = prettyPrint(fact.toString)
    val max = if ( value.length < 512 ) value.length else 512
    value.substring(0, max)
  }

  def enabled(reportLevel: Severity.Value, configLevel: Severity.Value): Boolean =
  {
    //use id to compare ints in order of declaration
    reportLevel.id <= configLevel.id
  }

  def report[ A ](fact: A): Unit =
  {
    report(fact, None, Severity.Debug)
  }

  def report[ A ](fact: A, level: Severity.Value): Unit =
  {
    report(fact, None, level)
  }

  def report[ A ](fact: A, exception: Throwable, level: Severity.Value): Unit =
  {
    report(fact, Option[Throwable](exception), level)
  }

  def report[ A ](fact: A, exception: Option[Throwable], level: Severity.Value): Unit =
  {
    trace(fact, exception, level)
    log(fact, exception, level)
  }

  def trace[ A ](fact: A): Unit =
  {
    trace(fact, None, Severity.Debug)
  }

  def trace[ A ](fact: A, level: Severity.Value): Unit =
  {
    trace(fact, None, level)
  }

  def trace[ A ](fact: A, exception: Throwable, level: Severity.Value): Unit =
  {
    trace(fact, Option[Throwable](exception), level)
  }

  def trace[ A ](fact: A, exception: Option[Throwable], level: Severity.Value): Unit =
  {
    if ( enabled(level, traceLevel) ) {
      //todo: worth adding severity to output <report> tag?
      println(header(level) + "\n" + wrap(fact).toString() + "\n")
      exception.map(_.printStackTrace)
    }
  }

  def log[ A ](fact: A): Unit =
  {
    log(fact, None, Severity.Debug)
  }

  def log[ A ](fact: A, level: Severity.Value): Unit =
  {
    log(fact, None, level)
  }

  def log[ A ](fact: A, exception: Throwable, level: Severity.Value): Unit =
  {
    log(fact, None, level)
  }

  def log[ A ](fact: A, exception: Option[Throwable], level: Severity.Value): Unit =
  {
    if ( enabled(level, logLevel) ) {
      val t:Throwable = exception.getOrElse(null)
      level match {
        case Severity.Fatal => {
          logger.log(Level.FATAL, fact.toString, t)
        }
        case Severity.Error => {
	        logger.log(Level.ERROR, fact toString, t)
        }
        case Severity.Warning => {
          logger.log(Level.WARN, fact toString, t)
        }
        case Severity.Info => {
	        logger.log(Level.INFO, fact toString, t)
        }
        case Severity.Debug => {
          logger.log(Level.DEBUG, fact toString, t)
        }
        case Severity.Trace => {
	        logger.log(Level.TRACE, fact toString, t)
        }
        case _ => {
          logger.log(Level.DEBUG, fact toString, t)
        }
      }
    }
  }

}
