// -*- mode: Scala;-*- 
// Filename:    VURI.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 20 19:10:01 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.msgs

import java.net.URI
import java.net.URL

trait URILike extends Serializable {
  def getScheme : String
  def getUserInfo : String
  def getAuthority : String
  def getHost : String
  def getPort : Int
  def getPath : String
  def getQuery : String
  def getFragment : String
  def uri : URI
}

class ConcreteURI(
  val scheme : String,
  val userInfo : Option[String],
  val authority : Option[String],
  val host : String,  
  val port : Option[Int],
  val path : String,
  val query : Option[String],
  val fragment : Option[String]
) extends URILike {
  def this(
    scheme : String, host : String, path : String, fragment : Option[String]) =
    this ( scheme, None, None, host, None, path, None, fragment )

  override lazy val uri =
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

  def significantBit( path : String ) = path

  override def equals( theOther : Any ) : Boolean = {
    (
      theOther match {  
        case that : URILike => {
          (
            scheme.equals( that.getScheme )
            && host.equals( that.getHost )
            && significantBit( path ).equals( significantBit( that.getPath ) )
            && fragment.equals( that.getFragment )
            && port.equals( that.getPort )
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
      + ( 37 * port.hashCode )
    )
  }
  
  override def getScheme : String = uri.getScheme
  override def getUserInfo : String = uri.getUserInfo
  override def getAuthority : String = uri.getAuthority
  override def getHost : String = uri.getHost
  override def getPort : Int = uri.getPort
  override def getPath : String = uri.getPath
  override def getQuery : String = uri.getQuery
  override def getFragment : String = uri.getFragment

  override def toString() : String = { "JURI(" + uri.toString + ")" }
}

object ConcreteURI {
  def apply(
    scheme : String,
    userInfo : Option[String],
    authority : Option[String],
    host : String,  
    port : Option[Int],
    path : String,
    query : Option[String],
    fragment : Option[String]
  ) : ConcreteURI =
    new ConcreteURI(
      scheme, userInfo, authority, host, port, path, query, fragment
    )
  def unapply(
    curi : ConcreteURI
  ) : Option[( String, Option[String], Option[String], String, Option[Int], String, Option[String], Option[String] )] = {
    Some( 
      (
        curi.scheme,
        curi.userInfo,
        curi.authority,
        curi.host,
        curi.port,
        curi.path,
        curi.query,
        curi.fragment
      )
    )
  }
}

case class JURI(
  override val scheme : String,
  override val userInfo : Option[String],
  override val authority : Option[String],
  override val host : String,  
  override val port : Option[Int],
  override val path : String,
  override val query : Option[String],
  override val fragment : Option[String]
) extends ConcreteURI(
  scheme, userInfo, authority, host, port, path, query, fragment
)

case class AgentURI(
  override val userInfo : Option[String],
  override val authority : Option[String],
  override val host : String,  
  override val port : Option[Int],
  override val path : String,
  override val query : Option[String],
  override val fragment : Option[String]
) extends ConcreteURI(
  "agent", userInfo, authority, host, port, path, query, fragment
)

case class AgentSessionURI(
  override val userInfo : Option[String],
  override val authority : Option[String],
  override val host : String,  
  override val port : Option[Int],
  override val path : String,
  override val query : Option[String],
  override val fragment : Option[String]
) extends ConcreteURI(
  "agent-session", userInfo, authority, host, port, path, query, fragment
)

object identityConversions {
  implicit def toURILike( url : URL ) : URILike = {
    JURI(
      url.getProtocol,
      ( url.getUserInfo match { case "" => None; case qry => Some( qry ) } ),
      ( url.getAuthority match { case "" => None; case qry => Some( qry ) } ),
      url.getHost,
      Some( url.getPort ),
      url.getPath,
      ( url.getQuery match { case "" => None; case qry => Some( qry ) } ),
      None
    )
  }
  implicit def toURILike( uri : URI ) : URILike = {
    JURI(
      uri.getScheme,
      ( uri.getUserInfo match { case "" => None; case qry => Some( qry ) } ),
      ( uri.getAuthority match { case "" => None; case qry => Some( qry ) } ),
      uri.getHost,
      Some( uri.getPort ),
      uri.getPath,
      ( uri.getQuery match { case "" => None; case qry => Some( qry ) } ),
      ( uri.getFragment match { case "" => None; case qry => Some( qry ) } )
    )
  }
  implicit def toURILike( s : String ) : URILike = {
    toURILike( new URI( s ) )
  }  
 
}
