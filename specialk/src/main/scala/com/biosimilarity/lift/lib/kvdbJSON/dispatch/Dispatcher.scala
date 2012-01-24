// -*- mode: Scala;-*- 
// Filename:    Dispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 23 22:23:35 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.kvdbJSON

import com.biosimilarity.lift.lib.kvdbJSON.Absyn.{ URI => kvdbURI, _ }
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import scala.collection.mutable.HashMap

import java.net.URI
import java.io.StringReader

trait KVDBJSONAPIDispatcherT {
  import scala.collection.JavaConversions._

  // comms
  def srcHost : String
  def srcExchange : String
  def srcScope : AMQPNodeJSStdScope =
    new AMQPNodeJSStdScope()
  lazy val stblSrcScope : AMQPNodeJSStdScope = srcScope
  def srcQM( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPNodeJSQueueM =
    new stblSrcScope.AMQPNodeJSQueueM( srcHost, srcExchange )
  def srcQM : stblSrcScope.AMQPNodeJSQueueM =
    srcQM( srcHost, srcExchange )
  lazy val stblSrcQM : stblSrcScope.AMQPNodeJSQueueM = srcQM
  def srcQ( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPQueue[String] = 
    srcQM( srcHost, srcExchange ).zeroJSON
  def srcQ : stblSrcScope.AMQPQueue[String] = stblSrcQM.zeroJSON

  // namespace
  def kvdbScope : PersistedTermStoreScope[String,String,String,String] = {
    throw new Exception( "TODO : implement kvdbScope on KVDBJSONAPIDispatcherT" )
  }
  lazy val stblKVDBScope : PersistedTermStoreScope[String,String,String,String] =
    kvdbScope
  def kvdbPersistenceScope : stblKVDBScope.PersistenceScope = {
    throw new Exception( "TODO : implement kvdbPersistenceScope on KVDBJSONAPIDispatcherT" )
  }
  lazy val stblKVDBPersistenceScope : stblKVDBScope.PersistenceScope =
    kvdbPersistenceScope
  def namespace : HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]
  = {
    throw new Exception( "TODO : implement namespace on KVDBJSONAPIDispatcherT" )
  }

  // service
  implicit def asString( kvdbURINetLoc : NetLocation ) : String = {
    kvdbURINetLoc match {
      case dnsAddr : DNSAddr => {
	dnsAddr.listdnselement_.toList match {
	  case elem :: elems => {
	    val elemStr =
	      elem match {
		case atmDNSElem : AtomDNSElement => {
		  atmDNSElem.lident_
		}
		case _ => {
		  throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURINetLoc )
		}
	      }
	    ( elemStr /: elems )(
	      ( acc, e ) => {
		val eStr = 
		  elem match {
		    case atmDNSElem : AtomDNSElement => {
		      atmDNSElem.lident_
		    }
		    case _ => {
		      throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURINetLoc )
		    }
		  }
		acc + "." + e
	      }
	    )
	  }
	  case _ => {
	    throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURINetLoc )
	  }
	}
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURINetLoc )
      }
    }
  }
  implicit def asString( kvdbURIScheme : URIScheme ) : String = {
    kvdbURIScheme match {
      case atmScheme : AtomScheme => {
	atmScheme.lident_
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI scheme in header" + kvdbURIScheme )
      }
    }
  }
  implicit def asInteger( kvdbURIPort : Port ) : Int = {
    kvdbURIPort match {
      case atmPort : AtomPort => {
	atmPort.integer_
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURIPort )
      }
    }
  }
  implicit def asHostPortTuple(
    kvdbURIRsrcLoc : URIRsrcLocation
  ) : ( String, Option[Int] ) = {
    kvdbURIRsrcLoc match {
      case rsrcPortLoc : URIRsrcPortLoc => {
	val hst : String = rsrcPortLoc.netlocation_			
	val prt : Int = rsrcPortLoc.port_ 
	( hst, Some( prt ) )
      }
      case rsrcLoc : URIRsrcLoc => {
	val hst : String = rsrcLoc.netlocation_
	( hst, None )
      }
    }
  }
  implicit def asString( kvdbURIPathElem : URIPathElement ) : String = {
    kvdbURIPathElem match {
      case atmPathElem : AtomPathElement => {
	atmPathElem.lident_
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI path in header" + kvdbURIPathElem )
      }
    }
  }
  implicit def asString( kvdbURIPath : URIRelativePath ) : String = {
    kvdbURIPath match {
      case spath : SlashPath => {
	spath.listuripathelement_.toList match {
	  case pElem :: pElems => {
	    val pE : String = pElem
	    ( pE /: pElems )(
	      ( acc : String, e : URIPathElement ) => {
		acc + "/" + e
	      }
	    )
	  }
	  case _ => ""
	}
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI path in header" + kvdbURIPath )
      }
    }
  }
  implicit def asURI( kvdbURI : kvdbURI ) : Option[URI] = {
    kvdbURI match {
      case bURI : BasicURI => {
	val scheme : String = bURI.urischeme_ 
	bURI.uripath_ match {
	  case lpath : LocatedtedPath => {
	    lpath.urilocation_ match {
	      case netLoc : URINetLocation => {
		val ( host : String, port : Option[Int] ) =
		  asHostPortTuple( netLoc.urirsrclocation_ )
		val path = lpath.urirelativepath_ 
		val lhost = port match {
		  case Some( p ) => 
		    host + p
		  case None => 
		    host
		}
		Some( new URI( scheme, lhost, path, "" ) )
	      }
	      case _ => {
		throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURI )
	      }
	    }
	  }
	  case _ => {
	    throw new Exception( "ill-formed kvdbJSON message: bad URI host in header" + kvdbURI )
	  }
	}
      }
      case nURU : NullURI => {
	None
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad URI in header " + kvdbURI )
      }
    }
  }
  
  def getRequest( lblrb : LblReqBody ) : KVDBRequest = {
    lblrb match {
      case kvdbLBLRB : KVDBLblReqBody => {
	kvdbLBLRB.kvdbrequest_
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad request body " + lblrb )
      }
    }
  }

  def getRequest( jrhb : KVDBJustReqHB ) : KVDBRequest = {
    getRequest( jrhb.lblreqbody_ )
  }

  def getRequest( jrbh : KVDBJustReqBH ) : KVDBRequest = {
    getRequest( jrbh.lblreqbody_ )
  }

  def dispatch(
    kvdb : stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction,
    req : KVDBRequest
  ) : Unit = {
    throw new Exception( "TODO : implement dispatch on KVDBJSONAPIDispatcherT" )
  }
  def dispatch( msg : Message ) : Unit = {
    msg match {
      case jreqHB : KVDBJustReqHB => {
	jreqHB.lblreqheader_ match {
	  case reqHdr : KVDBReqHdr => {
	    for( trgt <- asURI( reqHdr.uri_1 ); kvdb <- namespace.get( trgt ) ) {
	      dispatch( kvdb, getRequest( jreqHB ) )
	    }
	  }
	  case _ => {
	    throw new Exception( "ill-formed kvdbJSON message: bad header" + msg )
	  }
	}
      }
      case jreqBH : KVDBJustReqBH => {
	jreqBH.lblreqheader_ match {
	  case reqHdr : KVDBReqHdr => {
	    for( trgt <- asURI( reqHdr.uri_1 ); kvdb <- namespace.get( trgt ) ) {	      
	      dispatch( kvdb, getRequest( jreqBH ) )
	    }
	  }
	  case _ => {
	    throw new Exception( "ill-formed kvdbJSON message: bad header" + msg )
	  }
	}
      }
    }
  }
  def parse( msg : String ) : Message =
    (new parser( new Yylex( new StringReader( msg ) ) )).pMessage()

  def serveAPI( srcHost : String, srcExchange : String ) : Unit = {
    val apiSrcQM = srcQM( srcHost, srcExchange )
    val apiSrcQ = srcQ( srcHost, srcExchange )  
    
    for( msg <- apiSrcQM( apiSrcQ ) ) {
      dispatch( parse( msg ) )
    }
  }
  def serveAPI : Unit = serveAPI( srcHost, srcExchange )
}

case class KVDBJSONAPIDispatcher(
  override val srcHost : String,
  override val srcExchange : String
) extends KVDBJSONAPIDispatcherT
