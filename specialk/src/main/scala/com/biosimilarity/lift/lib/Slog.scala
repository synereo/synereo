// -*- mode: Scala;-*- 
// Filename:    Slog.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 11:17:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

//import net.lag.configgy._
//import net.lag.logging._
import com.typesafe.config._

import scala.annotation.elidable
import scala.annotation.elidable._
import scala.xml._
import scala.collection.mutable.HashMap
import java.util.UUID
import java.io.File

//import net.lag.configgy._

import org.apache.log4j.{PropertyConfigurator, Level, Logger}

object Severity extends Enumeration()
{
  type Severity = Value
  val Fatal, Error, Warning, Info, Debug, Trace = Value
}

trait WireTap {
  def tap [A] ( fact : A ) : Unit
}


object JournalIDVender extends UUIDOps

trait Verbosity {
    def id : UUID
  }
class Luddite(
  override val id : UUID
) extends Verbosity
class Blogger(
  override val id : UUID
) extends Luddite( id )
class Twitterer(
  override val id : UUID
) extends Blogger( id )

object Twitterer {
  def apply(
    id : UUID
  ) : Twitterer = new Twitterer( id )
  def unapply( t : Twitterer ) : Option[(UUID)] =
    Some( (t.id) )
}

object Blogger {
  def apply(
    id : UUID
  ) : Blogger = new Blogger( id )
  def unapply( t : Blogger ) : Option[(UUID)] =
    Some( (t.id) )
}

object Luddite {
  def apply(
    id : UUID
  ) : Luddite = new Luddite( id )
  def unapply( t : Luddite ) : Option[(UUID)] =
    Some( (t.id) )
}

case object TheTwitterer
extends Twitterer(
  JournalIDVender.getUUID
)
case object TheBlogger
extends Blogger(
  JournalIDVender.getUUID
)
case object TheLuddite
extends Luddite(
  JournalIDVender.getUUID
)
  

trait SeverityConversions {
  def SeverityFromOption(level: Option[ String ]): Severity.Value =
  {
    level match {
      case Some(x) => {
        SeverityFromString(x)
      }
      case None => {
        Severity.Trace
      }
    }
  }

  implicit def SeverityFromString(level: String): Severity.Value =
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
        Severity.Trace
      }
    }
  }
}

object LogConfiguration extends SeverityConversions
with Serializable {
  //Configgy.configure("log.conf")

  //@transient lazy val config = Configgy.config
  @transient lazy val config = 
    ConfigFactory.load( ConfigFactory.parseFile( new File( "log.conf" ) ) )

  var tweetLevel =
    try {
      SeverityFromString( config.getString( "tweetLevel" ) )
    }
    catch {
      case e => Severity.Trace
    }
  var blogLevel =
    try {
      SeverityFromString( config.getString( "blogLevel" ) )
    }
    catch {
      case e => Severity.Trace
    }

  @transient lazy val logger = lazyLoadLogger()

  def lazyLoadLogger() = {
    PropertyConfigurator.configure( "log.properties" )
    Logger.getLogger( this.getClass.getName )
  }
}

// This design is now officially baroque-en! Fix it into simplicity, please!
trait Journalist {
  object journalIDVender extends UUIDOps

  import LogConfiguration._    

  def prettyPrintElisions() : HashMap[String,String] =
    {
      val map = new HashMap[String,String]()
      map += ( "{" -> "" )
      map += ( "}" -> "" )
      map += ( "&amp;" -> "&" )
      map += ( "vamp;" -> "&" )
      map += ("amp;" -> "&" )
      map += ( "&quot;" -> "\"" )
      map += ( "vquot;" -> "\"" )
      map += ( "quot;" -> "\"" )
      map += ( "_-" -> "" )
      //map += ( ":" -> "" )
      map += ( "@class" -> "" )
      map += ( "com.biosimilarity.lift.model.store." -> "" )
      map += ( "com.protegra.agentservices.store." -> "" )
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
    "=" + level.toString.toUpperCase + " REPORT==== Thread " + Thread.currentThread.getName + " ==="
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

  var _loggingLevel : Option[Verbosity] = None
    def setLoggingLevel( verb : Verbosity ) : Unit = {
      _loggingLevel = Some( verb )
    }

  def exceptionToTraceStr( e : Exception ) : String = {
    val sw = new java.io.StringWriter()
    e.printStackTrace(
      new java.io.PrintWriter(
	sw,
	true
      )
    )
    sw.toString
  }

  @elidable(ALL)
  def tweetTrace( e : Exception ) = {
    reportage(exceptionToTraceStr( e ) )
  }
  def reportage[ A ](fact: A): Unit =
  {
    tweet(fact, Severity.Trace)
  }

  def reportage[ A ](fact: A, level: Severity.Value) =
  {
    tweet(fact, level)
  }

  @elidable(ALL)
  def tweet[ A ](fact: A): Unit =
  {
    tweet(fact, Severity.Trace)
  }

  def tweet[ A ](fact: A, level: Severity.Value) =
  {
    display(fact, level)
    blog(fact, level)
  }

  @elidable(ALL)
  def display[ A ](fact: A): Unit =
  {
    display(fact, Severity.Trace)
  }

  def display[ A ](fact: A, level: Severity.Value) =
  {
    if ( enabled(level, tweetLevel) ) {trace(fact, level)}
  }

  @elidable(ALL)
  private def trace[ A ](fact: A, level: Severity.Value) =
  {
    //todo: worth adding severity to output <report> tag?
    level match {
      case _ => {
        println(header(level) + "\n" + wrap(fact).toString() + "\n")
      }
    }
  }

  @elidable(ALL)
  def blog[ A ](fact: A): Unit =
  {
    blog(fact, Severity.Debug)
  }

  def blog[ A ](fact: A, level: Severity.Value) =
  {
    if ( enabled(level, blogLevel) ) {log(fact, level)}
  }

  @elidable(ALL)
  private def log[ A ](fact: A, level: Severity.Value) =
  {
    level match {
      case Severity.Fatal => {
        logger.log(Level.FATAL, fact toString)
      }
      case Severity.Error => {
        logger.log(Level.ERROR, fact toString)
      }
      case Severity.Warning => {
        logger.log(Level.WARN, fact toString)
      }
      case Severity.Info => {
        logger.log(Level.INFO, fact toString)
      }
      case Severity.Debug => {
        logger.log(Level.DEBUG, fact toString)
      }
      case Severity.Trace => {
        logger.log(Level.TRACE, fact toString)
      }
      case _ => {
        logger.log(Level.TRACE, fact toString)
      }
    }
  }

  //  implicit def exceptionToTraceStr( e : Exception ) : String = {
  //    val sw = new java.io.StringWriter()
  //    e.printStackTrace(
  //      new java.io.PrintWriter(
  //	sw,
  //	true
  //      )
  //    )
  //    sw.toString
  //  }

}

object ConfiguredJournalDefaults {
}

object BasicLogService extends Journalist
