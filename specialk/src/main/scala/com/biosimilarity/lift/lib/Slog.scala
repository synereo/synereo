// -*- mode: Scala;-*- 
// Filename:    Slog.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 11:17:09 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import net.lag.configgy._
import net.lag.logging._

import scala.xml._
import java.util.UUID

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
  

// This design is now officially baroque-en! Fix it into simplicity, please!
trait Journalist {

  object journalIDVender extends UUIDOps

  lazy val _notebook = new StringBuffer
  def notebook : StringBuffer = _notebook
  
  def displayFn[A] : ( Verbosity, A ) => Unit = {
    ( v : Verbosity, a : A ) => println( a )
  }

  def rememberFn[A] : ( Verbosity, A ) => Unit = {
    ( v : Verbosity, x : A ) => {
      notebook.append( x )
    }
  }

  def storeFn[A] : ( Verbosity, A ) => Unit = {
    ( v : Verbosity, x : A ) => {
      //throw new Exception( "log store not defined" )
    }
  }

  val reportage = report( TheTwitterer ) _

  case class TaggedFact[A]( verb : Verbosity, fact : A )

  def markUp[A]( verb : Verbosity)( fact : A ) =
    TaggedFact( verb, fact )

  def asTweet[A]( fact : A ) = markUp[A]( TheTwitterer )( fact )
  def asBlog[A]( fact : A ) = markUp[A]( TheBlogger )( fact )
  def asRecord[A]( fact : A ) = markUp[A]( TheLuddite )( fact )

  def tweet[A]( fact : A ) =
    report( Twitterer( journalIDVender.getUUID ) )( asTweet( fact ) )
  def blog[A]( fact : A ) =
    report( Blogger( journalIDVender.getUUID ) )( asTweet( fact ) )
  def record[A]( fact : A ) =
    report( Luddite( journalIDVender.getUUID ) )( asTweet( fact ) )

  implicit def exceptionToTraceStr( e : Exception ) : String = {
    val sw = new java.io.StringWriter()
    e.printStackTrace(
      new java.io.PrintWriter(
	sw,
	true
      )
    )
    sw.toString
  }

  def tweetTrace( e : Exception ) = {    
    report( Twitterer( journalIDVender.getUUID ) )(
      asTweet( exceptionToTraceStr( e ) ) 
    )
  }
  def blogTrace( e : Exception ) = {
    report( Blogger( journalIDVender.getUUID ) )(
      asTweet( exceptionToTraceStr( e ) )
    )
  }
  def recordTrace( e : Exception ) = {
    report( Luddite( journalIDVender.getUUID ) )(
      asTweet( exceptionToTraceStr( e ) )
    )
  }

  def tagIt [A]( verb : Verbosity, bite : A ) : Elem = {
    verb match {
      case Twitterer( _ ) => <tweet>{bite}</tweet>
      case Blogger( _ ) => <blog>{bite}</blog>
      case Luddite( _ ) => <record>{bite}</record>
    }
  }

  def report [A] ( verb : Verbosity )( fact : A ) : Unit = {
    fact match {
      case TaggedFact( vrb, bite ) => {
	val rpt = tagIt( vrb, bite )
	if ( verb.getClass.isInstance( vrb ) ) {
	  verb match {
	    case Twitterer( _ ) => {	      
	      displayFn( vrb, rpt )
	      rememberFn( vrb, rpt )
	      storeFn( vrb, rpt )
	    }
	    case Blogger( _ ) => {
	      rememberFn( vrb, rpt )
	      storeFn( vrb, rpt )
	    }
	    case Luddite( _ ) => {
	      storeFn( vrb, rpt )
	    }
	  }
	}
      }
      case _ => {
	val rpt = tagIt( verb, fact )
	verb match {
	  case Twitterer( _ ) => {	    
	    displayFn( verb, rpt )
	    rememberFn( verb, rpt )
	    storeFn( verb, rpt )
	  }
	  case Blogger( _ ) => {
	    rememberFn( verb, rpt )
	    storeFn( verb, rpt )
	  }
	  case Luddite( _ ) => {
	    storeFn( verb, rpt )
	  }
	}
      }
    }    
  }
}

trait ConfiggyReporting {
  self : Journalist =>
  
  def config : Config
  def logger : Logger

  def wrap [A] ( fact : A ) : Elem = {
     <report>{fact}</report>
  }

  def logFatal [A] ( fact : A ) : Unit = {
    logger.ifFatal(wrap(fact) toString)
  }

  def logCritical [A] ( fact : A ) : Unit = {
    logger.ifCritical(wrap(fact) toString)
  }

  def logError [A] ( fact : A ) : Unit = {
    logger.ifError(wrap(fact) toString)
  }
  
  def logWarning [A] ( fact : A ) : Unit = {
    logger.ifWarning(wrap(fact) toString) 
  }
  
  def logInfo [A] ( fact : A ) : Unit = {
    logger.ifInfo(wrap(fact) toString) 
  }
  
  def logDebug [A] ( fact : A ) : Unit = {
    logger.ifDebug(wrap(fact) toString) 
  }

  def logTrace [A] ( fact : A ) : Unit = {
    logger.ifTrace(wrap(fact) toString)
  }
}

trait ConfiggyJournal {
  self : Journalist with ConfiggyReporting =>
    Configgy.configure("log.conf")

  override lazy val config = Configgy.config

  override lazy val logger = Logger.get  

  override def storeFn[A] : ( Verbosity, A ) => Unit = {
    ( v : Verbosity, a : A ) => {
      v match {
	case Twitterer( _ ) => {
	  logger.ifInfo( tagIt( v, a ).toString ) 
	}
	case Blogger( _ ) => {
	  logger.ifTrace( tagIt( v, a ).toString ) 
	}
	case Luddite( _ ) => {
	  logger.ifDebug( tagIt( v, a ).toString ) 
	}
      }
    }
  }
}

object ConfiguredJournalDefaults {
  val loggingLevel = "Tweet"
}

trait ConfiguredJournal {
  self : Journalist
       with ConfiggyReporting
	with ConfigurationTrampoline =>    

  override lazy val config = Configgy.config

  override lazy val logger = Logger.get  

  override def configurationDefaults : ConfigurationDefaults = {
    ConfiguredJournalDefaults.asInstanceOf[ConfigurationDefaults]
  }

  def loggingLevel        : String =
    configurationFromFile.get( "loggingLevel" ).getOrElse( bail() )

  var _loggingLevel : Option[Verbosity] = None
  def setLoggingLevel( verb : Verbosity ) : Unit = {
    _loggingLevel = Some( verb )
  }
  def getLoggingLevel : Verbosity = {
    _loggingLevel match {
      case Some( verb ) => verb
      case None => {
	loggingLevel match {
	  case "Tweet" => {
	    val ll = Twitterer( journalIDVender.getUUID )
	    _loggingLevel = Some( ll )
	    ll
	  }
	  case "Blog" => {
	    val ll = Blogger( journalIDVender.getUUID )
	    _loggingLevel = Some( ll )
	    ll
	  }
	  case "Record" => {
	    val ll = Luddite( journalIDVender.getUUID )
	    _loggingLevel = Some( ll )
	    ll
	  }
	}
      }
    }
  }

  override def tweet[A]( fact : A ) =
    report( getLoggingLevel )( asTweet( fact ) )
  override def blog[A]( fact : A ) =
    report( getLoggingLevel )( asTweet( fact ) )
  override def record[A]( fact : A ) =
    report( getLoggingLevel )( asTweet( fact ) )

  override def storeFn[A] : ( Verbosity, A ) => Unit = {
    ( v : Verbosity, a : A ) => {
      v match {
	case Twitterer( _ ) => {
	  logger.ifInfo( tagIt( v, a ).toString ) 
	}
	case Blogger( _ ) => {
	  logger.ifTrace( tagIt( v, a ).toString ) 
	}
	case Luddite( _ ) => {
	  logger.ifDebug( tagIt( v, a ).toString ) 
	}
      }
    }
  }
}

abstract class Reporter( override val notebook : StringBuffer )
	 extends Journalist

class ConfiggyReporter(
  override val notebook : StringBuffer
) extends Reporter( notebook )
  with Journalist
  with ConfiggyReporting	 
  with ConfiggyJournal {
}
