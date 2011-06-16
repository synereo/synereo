// -*- mode: Scala;-*- 
// Filename:    Moniker.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jun 15 05:32:56 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.moniker
import java.net.URI
import java.net.URL

trait Moniker
case class MURI( uri : URI ) extends Moniker
case class MURN( uri : URI ) extends Moniker
case class MURL( url : URL ) extends Moniker

class URM(
  val scheme : String,
  val userInfo : Option[String],
  val authority : Option[String],
  val host : String,  
  val port : Option[Int],
  val path : String,
  val query : Option[String],
  val fragment : Option[String]
) extends Moniker {
  lazy val uri =
    {
      val qry = query.getOrElse( "" )
      val frg = fragment.getOrElse( "" )
	      
      userInfo match {
	case Some( usr ) => {
	  port match {
	    case Some( p ) => {
	      new URI(
		scheme,
		usr,
		host,
		p,
		path,
		qry,
		frg
	      )
	    }
	    case _ => {
	      throw new Exception(
		"invalid arguments: userInfo without port"
	      )
	    }
	  }
	}
	case _ => {
	  authority match {
	    case Some( auth ) => {
	      new URI(
		scheme,
		auth,
		path,
		qry,
		frg
	      )
	    }
	    case _ => {
	      new URI( scheme, host, path, frg )
	    }
	  }	  
	}
      }
    }
  def significantBit( path : String ) = ""
  override def equals( theOther : Any ) : Boolean = {
    (
      theOther match {
	case that : URM => {
	  (
	    scheme.equals( that.scheme )
	    && host.equals( that.host )
	    && significantBit( path ).equals( significantBit( that.path ) )
	    && fragment.equals( that.fragment )
	  )
	}
	case _ => false
      }
    )
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * scheme.hashCode )
      + ( 37 * host.hashCode )
      + ( 37 * significantBit( path ).hashCode )
      + ( 37 * fragment.hashCode )
    )
  }
}


object identityConversions {
  implicit def toMoniker( url : URL ) : Moniker = {
    new MURL( url )
  }
  implicit def toMoniker( uri : URI ) : Moniker = {
    new MURI( uri )
  }
  implicit def toMoniker( s : String ) : Moniker = {
    new URI( s )
  }  
  implicit def toURI( mnkr : Moniker ) : URI = {
    mnkr match {
      case muri : MURI => muri.uri
      case murn : MURN => murn.uri
      case _ => throw new Exception( "conversion not defined" )
    }
  }
  implicit def toURL( mnkr : Moniker ) : URL = {
    mnkr match {
      case murl : MURL => murl.url
      case _ => throw new Exception( "conversion not defined" )
    }
  }
}
