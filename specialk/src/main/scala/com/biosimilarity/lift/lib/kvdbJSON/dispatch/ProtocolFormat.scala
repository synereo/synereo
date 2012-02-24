// -*- mode: Scala;-*- 
// Filename:    ProtocolFormat.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb  2 14:06:18 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.kvdbJSON

import com.biosimilarity.lift.lib.kvdbJSON.Absyn.{
  URI => kvdbURI, UUID => kvdbUUID, Message => kvdbMessage, _
}

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store._
//import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import scala.util.continuations._
import scala.collection.mutable.HashMap
import scala.xml._

import org.prolog4j._
import biz.source_code.base64Coder.Base64Coder

import java.net.URI
import java.io.StringReader
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

trait KVDBJSONAPIProtocolFormatT {
  import scala.collection.JavaConversions._
  
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

  implicit def asOptString( uuid : kvdbUUID ) : Option[String] = {
    uuid match {
      case primUUID : KVDBPrimUUID => Some( primUUID.primuuid_ )
      case nullUUID : KVDBNullUUID => None
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
		val rpath = lpath.urirelativepath_ 
		val path = 
		  rpath match {
		    case spath : SlashPath => {
		      spath.listuripathelement_.toList match {
			case pElem :: pElems => {
			  var f = 
			    pElem match {
			      case atmPE : AtomPathElement => atmPE.lident_
			      case _ => {
				throw new Exception( "bad URI format: unexpected path element" )
			      }
			    }

			  ( f /: pElems )(
			    ( acc, e ) => {
			      val c : String = 
				e match {
				  case atmPE : AtomPathElement => atmPE.lident_
				  case _ => {
				    throw new Exception( "bad URI format: unexpected path element" )
				  }
				}
			      acc + "/" + c
			    }
			  )
			}
			case _ => ""
		      }
		    }
		  }
		val lhost = port match {
		  case Some( p ) => 
		    host + ":" + p
		  case None => 
		    host
		}

		Some( new URI( scheme, lhost, "/" + path, null ) )
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

  def jsonToTerm( qryElem : QryGrndLit ) : CnxnCtxtLabel[String,String,String] with Factual = {
    qryElem match {
      case qstr : QStr => {
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String]( qstr.string_ )
	)
      }
      case qnum : QNum => {
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String] (
	    qnum match {
	      case qint : QInt => {
		qint.integer_ + ""
	      }
	      case qdbl : QDbl => {
		qdbl.double_ + ""
	      }
	      case _ => {
		throw new Exception( "ill-formed message: bad num " + qnum )
	      }
	    }
	  )
	)
      }
      case qbool : QBool => {
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String](
	    qbool match {
	      case qtru : QTru => "true"
	      case qfal : QFal => "false"
	      case _ => {
		throw new Exception( "ill-formed message: bad boolean " + qbool )
	      }
	    }
	  )
	)
      }
      case qnul : QNul => {
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String]( "null" )
	)
      }
    }
  }

  def jsonToTerm( qryElem : QryElem ) : CnxnCtxtLabel[String,String,String] with Factual = {
    qryElem match {
      case qvar : QVar => {
	new CnxnCtxtLeaf[String,String,String](
	  Right[String,String]( qvar.varuident_ )
	)
      }
      case qval : QVal => {
	qval.qryvalue_ match {
	  case qatm : QAtomic => {
	    jsonToTerm( qatm.qrygrndlit_ )
	  }
	  case qcoll : QColl => {
	    new CnxnCtxtBranch[String,String,String](
	      "qCollection", jsonToTerm( qcoll.qryarray_ )
	    )
	  }
	  case qcomp : QComp => {
	    jsonToTerm( qcomp.qryterm_ )
	  }
	}
      }
      case _ => {
	throw new Exception( "ill-formatted qryElem " + qryElem )
      }
    }
  }

  def jsonToTerm(
    qryArray : QryArray
  ) : List[CnxnCtxtLabel[String,String,String] with Factual] = {
    qryArray match {
      case qarray : QArray => {
	qarray.listqryelem_.toList.map( jsonToTerm )
      }
      case _ => {
	throw new Exception( "ill-formatted term array " + qryArray )
      }
    }
  }

  def jsonToTerm( qtrm : QryTerm ) : CnxnCtxtLabel[String,String,String] with Factual = {
    qtrm match {
      case qt : QTerm => {
	new CnxnCtxtBranch[String,String,String](
	  qt.string_,
	  jsonToTerm( qt.qryarray_ )
	)
      }
      case _ => {
	throw new Exception( "ill-formatted term " + qtrm )
      }
    }
    
  }

  def jsonToTerm( json : Pattern ) : CnxnCtxtLabel[String,String,String] with Factual = {
    json match {
      case qp : QPointed => {
	qp.qryterm_ match {
	  case qtrm : QTerm => {
	    jsonToTerm( qtrm )
	  }
	  case _ => {
	    throw new Exception( "ill-formatted pattern term " + json )
	  }
	}
      }
      case _ => {
	throw new Exception( "ill-formatted pattern " + json )
      }
    }
  }

  def asPattern( kvdbAskReq : KVDBAskReq ) : CnxnCtxtLabel[String,String,String] with Factual = {
    kvdbAskReq.askreqpacket_ match {
      case askReqData : KVDBAskReqData => {
	jsonToTerm( askReqData.pattern_ )
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad ask request data " + kvdbAskReq )
      }
    }
  }

  def asPattern( kvdbTellReq : KVDBTellReq ) : CnxnCtxtLabel[String,String,String] with Factual = {
    kvdbTellReq.tellreqpacket_ match {
      case tellReqData : KVDBTellReqData => {
	jsonToTerm( tellReqData.pattern_ )
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad ask request data " + kvdbTellReq )
      }
    }
  }

  def patternVars(
    pattern : CnxnCtxtLabel[String,String,String]
  ) : List[String] = {
    ( ( Nil : List[String] ) /: pattern.atoms )(
      ( acc : List[String], atom : Either[String,String] ) => {
	atom match {
	  case Right( v ) => acc ++ List( v )
	  case _ => acc
	}
      }
    )
  }

  // BUGBUG : lgm -- is it really necessary to create a new parser to
  // parse each message?
  def parse( msg : String ) : kvdbMessage =
    (new parser( new Yylex( new StringReader( msg ) ) )).pMessage()  
}
