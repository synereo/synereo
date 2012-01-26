// -*- mode: Scala;-*- 
// Filename:    Dispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 23 22:23:35 2012 
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

trait KVDBJSONAPIDispatcherT
extends Serializable
with CnxnXQuery[String,String,String]
with CnxnXML[String,String,String]
with CnxnCtxtInjector[String,String,String]
with Blobify
with Journalist
with ConfiggyReporting
with ConfiguredJournal
with ConfigurationTrampoline
with UUIDOps {
  import scala.collection.JavaConversions._
  
  // Configuration
  override def configFileName : Option[String] = None
  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

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

  def replyScope : AMQPNodeJSStdScope =
    new AMQPNodeJSStdScope()
  lazy val stblReplyScope : AMQPNodeJSStdScope = replyScope
  def replyQM( replyHost : String, replyExchange : String ) : stblReplyScope.AMQPNodeJSQueueM =
    new stblReplyScope.AMQPNodeJSQueueM( replyHost, replyExchange )
  def replyQ( replyHost : String, replyExchange : String ) : stblReplyScope.AMQPQueue[String] = 
    replyQM( replyHost, replyExchange ).zeroJSON

  // namespace
  def kvdbScope : PersistedTermStoreScope[String,String,String,String]
  lazy val stblKVDBScope : PersistedTermStoreScope[String,String,String,String] =
    kvdbScope
  def kvdbPersistenceScope : stblKVDBScope.PersistenceScope
  lazy val stblKVDBPersistenceScope : stblKVDBScope.PersistenceScope =
    kvdbPersistenceScope

  // this controls what URI's will be dispatched to KVDB's
  def namespace : HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]

  type ReplyTrgt =
    Either[( String, String ),( stblReplyScope.AMQPNodeJSQueueM, stblReplyScope.AMQPQueue[String] )]

  // this controls what URI's will be dispatched to with results
  def replyNamespace : HashMap[URI,ReplyTrgt]

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

		Some( new URI( scheme, lhost, "/" + path, "" ) )
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

  def asValue( kvdbTellReq : KVDBTellReq ) : stblKVDBScope.mTT.Resource = {
    kvdbTellReq.tellreqpacket_ match {
      case tellReqData : KVDBTellReqData => {
	tellReqData.blob_ match {
	  case qblob : QBlob => {
	    stblKVDBScope.mTT.Ground( qblob.string_ )
	  }
	}
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

  def substAsJSON(
    pattern : CnxnCtxtLabel[String,String,String],
    rsrc : stblKVDBScope.mTT.Ground
  ) : Option[String] = {
    None
  }

  def substAsJSONPairs(
    ptrnVars : List[String],
    rsrc : stblKVDBScope.mTT.RBound
  ) : Option[String] = {
    def resolve( subst : Solution[String], pVar : String ) : String = {
      subst.on( pVar )
      subst.get( )
    }

    def asJSONPairs(
      subst : Solution[String],
      ptrnVars : List[String]
    ) : String = {
      ptrnVars match {
	case pVar :: pVars => {
	  ( ( pVar + " : " + resolve( subst, pVar ) ) /: pVars )(
	    ( acc, e ) => {
	      acc + " , " + e + " : " + resolve( subst, e )
	    }
	  )
	}
	case _ => {
	  ""
	}
      }
    }

    rsrc match {
      case stblKVDBScope.mTT.RBound( Some( rsrc ), Some( subst ) ) => {
	rsrc match {
	  case bndRsrc : stblKVDBScope.mTT.RBound => {
	    Some(
	      (
		asJSONPairs( subst, ptrnVars )
		+ " , "
		+ substAsJSONPairs( ptrnVars, bndRsrc ).getOrElse( "" )
	      )
	    )
	  }
	}
      }
      case stblKVDBScope.mTT.RBound( None, Some( subst ) ) => {
	Some( asJSONPairs( subst, ptrnVars ) )
      }
      case stblKVDBScope.mTT.RBound( _, None ) => {
	None
      }
    }
  }

  def getGrndVal( 
    rsrc : stblKVDBScope.mTT.Resource
  ) : Option[String] = {
    rsrc match {
      case stblKVDBScope.mTT.Ground( v ) => Some( v + "" )
      case stblKVDBScope.mTT.RBound( oRsrc, _ ) => {
	for( bndRsrc <- oRsrc; gv <- getGrndVal( bndRsrc ) ) yield {
	  gv
	}
      }
      case _ => {
	throw new Exception( "unhandled resource case while getting value: " + rsrc )
      }
    }
  }

  def asResponse(
    reqHdr : KVDBReqHdr,
    pattern : CnxnCtxtLabel[String,String,String],
    oRsrc : Option[stblKVDBScope.mTT.Resource]
  ) : String = {
    val rspMsgId = getUUID
    val headers =
      for(
	reqMsgId <- asOptString( reqHdr.uuid_1 ); 
	reqFlowId <- asOptString( reqHdr.uuid_2 );
	reqSrcURI <- asURI( reqHdr.uri_1 );
	reqTrgtURI <- asURI( reqHdr.uri_2 )
      ) yield {
	(
	  "[ "
	  + reqSrcURI + ","
	  + reqTrgtURI + ","
	  + rspMsgId + ","
	  + reqFlowId + ","
	  + reqMsgId
	  + " ]"
	)
      }

    val body =
      for( rsrc <- oRsrc ) yield {
	val reqPtrn = toJSON( pattern )	
	val ( rspVal, rspSubst ) =
	  rsrc match {
	    case stblKVDBScope.mTT.Ground( v ) => {
	      ( v + "", "null" )
	    }
	    case bndRsrc : stblKVDBScope.mTT.RBound => {
	      (
		getGrndVal( rsrc ), 
		substAsJSONPairs( patternVars( pattern), bndRsrc ) match {
		  case Some( jps ) => {
		    "{ " + jps + " }"
		  }
		  case _ => "null" 
		}
	      )
	    }
	    case _ => {
	      throw new Exception( "unhandled resource case: " + rsrc )
	    }
	  }

	( "{ "
	 + "\"answer\"" + ":"
	 + "[ "
	 + reqPtrn + ","
	 + rspSubst + ","
	 + rspVal
	 + " ]"
	 + " }"      
       )
      }

    (
      "{ "
      + "\"headers\"" + ":"
      + headers.getOrElse( "null" )
      + "\"body\"" + ":"
      + body.getOrElse( "null" )
      + " }"
    )
  }

  def craftResponse( reqHdr : ReqHeader, kvdbTellReq : KVDBTellReq ) : String = {
    (
      "{ " 
      + "\"acknowledge\"" + " : "
      + "\"ok\""
      + " }"
    )
  }

  def asReplyTrgt(
    reqHdr : KVDBReqHdr
  ) : Option[( stblReplyScope.AMQPNodeJSQueueM, stblReplyScope.AMQPQueue[String] )] = {
    for( trgt <- asURI( reqHdr.uri_2 ); rplyTrgt <- replyNamespace.get( trgt ) ) yield {
      rplyTrgt match {
	case Left( ( replyHost, replyExchange ) ) => {
	  val replyM = replyQM( replyHost, replyExchange )
	  val replyQ = replyM.zeroJSON
	  ( replyM, replyQ )
	}
	case Right( ( replyM, replyQ ) ) => {
	  ( replyM, replyQ )
	}
      }
    }
  }

  def dispatch(
    kvdb : stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction,
    reqHdr : ReqHeader,
    req : KVDBRequest
  ) : Unit = {
    tweet( "dispatching " + req + " to + " + kvdb )
    reqHdr match {
      case kvdbReqHdr : KVDBReqHdr => {
	for( ( m, q ) <- asReplyTrgt( kvdbReqHdr ) ) {
	  req match {
	    case askReq : KVDBAskReq => {	      
	      val pattern = asPattern( askReq )
	      tweet( "we have an ask request " + pattern + " from " + kvdb )
	      reset {
		for( rslt <- kvdb.get( pattern ) ) {
		  q ! asResponse( kvdbReqHdr, pattern, rslt )
		}
	      }
	    }
	    case tellReq : KVDBTellReq => {
	      val pattern = asPattern( tellReq )
	      val value = asValue( tellReq )
	      tweet( "we have a tell request to put " + value + " at " + pattern + " in " + kvdb )
	      reset {
		kvdb.put( pattern, value )
		q ! craftResponse( reqHdr, tellReq )
	      }
	    }
	  }
	}
      }
      case _ => {
	throw new Exception( "ill-formed kvdbJSON message: bad header " + reqHdr )
      }
    }    
  }  

  def dispatch( msg : kvdbMessage ) : Unit = {
    tweet( "dispatching " + msg )
    msg match {
      case jreqHB : KVDBJustReqHB => {
	tweet( msg + " is a KVDBJustReqHB " )
	jreqHB.lblreqheader_ match {
	  case lblReqHdr : KVDBLblReqHdr => {
	    tweet( msg + " has a KVDBLblReqHdr " + lblReqHdr )
	    lblReqHdr.reqheader_ match {
	      case reqHdr : KVDBReqHdr => {
		tweet( msg + " has a reqHdr " + reqHdr )		
		for( trTrgt <- asURI( reqHdr.uri_1 ) ) {
		  tweet( msg + "'s trgt URI is " + trTrgt )
		  for( kvdb <- namespace.get( trTrgt ) ) {
		    tweet( msg + "'s trgt kvdb is " + kvdb )
		  }
		}
		for( trgt <- asURI( reqHdr.uri_1 ); kvdb <- namespace.get( trgt ) ) {
		  tweet( "dispatching to trgt " + trgt + " corresponding to " + kvdb )
		  dispatch( kvdb, reqHdr, getRequest( jreqHB ) )
		}
	      }
	    }	    
	  }

	  // BUGBUG : lgm -- this is a work around due to ambiguity
	  // arising from non-ordering of elements in a JSON object
	  case rspHdr : KVDBLblRspHdr => {
	    throw new Exception( "ambiguity: bad header " + rspHdr )
	  }
	  case hdr@_ => {
	    throw new Exception(
	      "ill-formed kvdbJSON message: bad header " + msg + "\nheader: " + hdr
	    )
	  }
	}
      }
      case jreqBH : KVDBJustReqBH => {
	tweet( msg + " is a KVDBJustReqBH " )
	jreqBH.lblreqheader_ match {
	  case lblReqHdr : KVDBLblReqHdr => {
	    tweet( msg + " has a KVDBLblReqHdr " + lblReqHdr )
	    lblReqHdr.reqheader_ match {
	      case reqHdr : KVDBReqHdr => {
		tweet( msg + " has a reqHdr " + reqHdr )	  
		for( trTrgt <- asURI( reqHdr.uri_1 ) ) {
		  tweet( msg + "'s trgt URI is " + trTrgt )
		  for( kvdb <- namespace.get( trTrgt ) ) {
		    tweet( msg + "'s trgt kvdb is " + kvdb )
		  }
		}
		
		
		for( trgt <- asURI( reqHdr.uri_1 ); kvdb <- namespace.get( trgt ) ) {	      
		  tweet( "dispatching to trgt " + trgt + " corresponding to " + kvdb )
		  dispatch( kvdb, reqHdr, getRequest( jreqBH ) )
		}
	      }
	    }	    
	  }
	  case _ => {
	    throw new Exception( "ill-formed kvdbJSON message: bad header" + msg )
	  }
	}
      }
    }
  }

  // BUGBUG : lgm -- is it really necessary to create a new parser to
  // parse each message?
  def parse( msg : String ) : kvdbMessage =
    (new parser( new Yylex( new StringReader( msg ) ) )).pMessage()

  def serveAPI( srcHost : String, srcExchange : String ) : Unit = {
    tweet( "serving api on " + srcHost + " from amqp exchange " + srcExchange )
    val apiSrcQM = srcQM( srcHost, srcExchange )
    val apiSrcQ = srcQ( srcHost, srcExchange )  
    
    for( msg <- apiSrcQM( apiSrcQ ) ) {
      tweet( "received: " + msg )
      dispatch( parse( msg ) )
    }
  }

  def serveAPI : Unit = serveAPI( srcHost, srcExchange )
}

class KVDBJSONAPIDispatcher(
  override val srcHost : String,
  override val srcExchange : String
) extends KVDBJSONAPIDispatcherT {
  override def kvdbScope : PersistedTermStoreScope[String,String,String,String] = PTSS
  override def kvdbPersistenceScope : stblKVDBScope.PersistenceScope = {
    // This cast makes me sad...
    PTSS.Being.asInstanceOf[stblKVDBScope.PersistenceScope]
  }

  // this controls what URI's will be dispatched to KVDB's
  override def namespace : HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]
  = {
    stblNamespace match {
      case Some( ns ) => ns
      case _ => {
	val ns = new HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]()
	stblNamespace = Some( ns )
	ns
      }
    }
  }
  @transient var stblNamespace : Option[HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]] = None

  // this controls what URI's will be dispatched to with results to db requests
  override def replyNamespace : HashMap[URI,ReplyTrgt] = {
    stblReplyNamespace match {
      case Some( rns ) => rns
      case _ => {
	val rns = new HashMap[URI,ReplyTrgt]()
	stblReplyNamespace = Some( rns )
	rns
      }
    }
    
  }
  @transient var stblReplyNamespace : Option[HashMap[URI,ReplyTrgt]] = None

  def addSingletonKVDB( uri : URI, db : String, host : String ) : Unit = {
    // This cast makes me sad...
    val kvdb = PTSS.singleton( db, host ).asInstanceOf[stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]
    namespace += ( ( uri, kvdb ) )
  }

  def addSingletonKVDB( uri : URI ) : Unit = {
    val host = uri.getHost
    val path = uri.getPath
    val db = path.split( "/" )( 1 )
    tweet( "adding a kvdb@" + host + " to " + uri + " storing in " + db + "." )
    // This cast makes me sad...
    val kvdb = PTSS.singleton( db, host ).asInstanceOf[stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]

    namespace += ( ( uri, kvdb ) )
  }
  
  def addPtToPtKVDB(
    srcURI : URI, srcHost : String,
    trgtURI : URI, trgtHost : String,
    db : String
  ) : Unit = {
    // This cast makes me sad...
    val kvdb = PTSS.ptToPt( db, srcHost, trgtHost ).asInstanceOf[stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]

    namespace += ( ( srcURI, kvdb ) )
    namespace += ( ( trgtURI, kvdb ) )
  }

  def addPtToPtKVDB( srcURI : URI, trgtURI : URI ) : Unit = {
    val srcHost = srcURI.getHost
    val srcPath = srcURI.getPath
    val srcDB = srcPath.split( "/" )( 1 )

    val trgtHost = trgtURI.getHost
    val trgtPath = trgtURI.getPath
    val trgtDB = trgtPath.split( "/" )( 1 )

    val db = 
      if ( srcDB.equals( trgtDB ) ) {
	srcDB
      }
      else {
	// BUGBUG : lgm -- issue warning to log!
	srcDB
      }

    // This cast makes me sad...
    val kvdb = PTSS.ptToPt( db, srcHost, trgtHost ).asInstanceOf[stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]

    namespace += ( ( srcURI, kvdb ) )
    namespace += ( ( trgtURI, kvdb ) )
  }

  def addReplyQueue( uri : URI, host : String, exchange : String ) : Unit = {
    replyNamespace +=
    ( ( uri, Left[( String, String ),( stblReplyScope.AMQPNodeJSQueueM, stblReplyScope.AMQPQueue[String] )]( host, exchange ) ) )
  }

  object PTSS
       extends PersistedTermStoreScope[String,String,String,String] 
       with UUIDOps
       with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT

    object Being extends PersistenceScope with Serializable {      
      
      override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]

      object theEMTypes extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
       with Serializable
      {
	case class PrologSubstitution( soln : Solution[String] )
	   extends Function1[mTT.Resource,Option[mTT.Resource]] {
	     override def apply( rsrc : mTT.Resource ) = {
	       Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	override type Substitution = PrologSubstitution
      }      

      override def protoEMTypes : EMTypes =
	theEMTypes

      class PersistedStringMGJ(
	val dfStoreUnitStr : String,
	override val name : Moniker,
	override val acquaintances : Seq[Moniker]
      ) extends PersistedMonadicGeneratorJunction(
	name, acquaintances
      ) {      
	def this() = {
	  this(
	    "",
	    MURI( new URI( "agent", "localhost", "/connect", "" ) ),
	    Nil
	  )
	}

	class StringXMLDBManifest(
          override val storeUnitStr : String,
          @transient override val labelToNS : Option[String => String],
          @transient override val textToVar : Option[String => String],
          @transient override val textToTag : Option[String => String]
	)
	extends XMLDBManifest( database ) {
	  override def valueStorageType : String = {
	    throw new Exception( "valueStorageType not overriden in instantiation" )
	  }
	  override def continuationStorageType : String = {
	    throw new Exception( "continuationStorageType not overriden in instantiation" )
	  }

	  override def storeUnitStr[Src,Label,Trgt](
	    cnxn : Cnxn[Src,Label,Trgt]
	  ) : String = {     
	    cnxn match {
	      case CCnxn( s, l, t ) =>
		s.toString + l.toString + t.toString
	    }	    
	  }	
	  
	  def kvNameSpace : String = "record"
	  def kvKNameSpace : String = "kRecord"
	  
	  def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
	    ns1.equals( ns2 )
	  }
	  
	  override def asStoreValue(
	    rsrc : mTT.Resource
	  ) : CnxnCtxtLeaf[String,String,String] with Factual = {
	    tweet(
	      "In asStoreValue on " + this + " for resource: " + rsrc
	    )
	    val storageDispatch = 
	      rsrc match {
		case k : mTT.Continuation => {
		  tweet(
		    "Resource " + rsrc + " is a continuation"
		  )
		  continuationStorageType
		}
		case _ => {
		  tweet(
		    "Resource " + rsrc + " is a value"
		  )
		  valueStorageType
		}
	      };

	    tweet(
	      "storageDispatch: " + storageDispatch
	    )

	    val blob =
	      storageDispatch match {
		case "Base64" => {
                  println("start base64")

		  val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
		  val oos : ObjectOutputStream = new ObjectOutputStream( baos )
		  oos.writeObject( rsrc.asInstanceOf[Serializable] )
		  oos.close()
                  println("end base64")
		  new String( Base64Coder.encode( baos.toByteArray() ) )
		}
		case "CnxnCtxtLabel" => {
		  tweet(
		    "warning: CnxnCtxtLabel method is using XStream"
		  )
		  toXQSafeJSONBlob( rsrc )		  		  
		}
		case "XStream" => {
		  tweet(
		    "using XStream method"
		  )
		  
		  toXQSafeJSONBlob( rsrc )
		}
		case _ => {
		  throw new Exception( "unexpected value storage type" )
		}
	    }
	    new CnxnCtxtLeaf[String,String,String](
	      Left[String,String]( blob )
	    )
	  }
	  
	  def asCacheValue(
	    ccl : CnxnCtxtLabel[String,String,String]
	  ) : String = {
	    tweet(
	      "converting to cache value"
	    )
	    ccl match {
	      case CnxnCtxtBranch(
		"string",
		CnxnCtxtLeaf( Left( rv ) ) :: Nil
	      ) => {
		val unBlob =
		  fromXQSafeJSONBlob( rv )
		
		unBlob match {
		  case rsrc : mTT.Resource => {
		    getGV( rsrc ).getOrElse( "" )
		  }
		}
	      }
	      case _ => {
		asPatternString( ccl )
	      }
	    }
	  }
	 
	  override def asResource(
	    key : mTT.GetRequest, // must have the pattern to determine bindings
	    value : Elem
	  ) : emT.PlaceInstance = {
	    val ttt = ( x : String ) => x
	    
	    val ptn = asPatternString( key )
	    println( "ptn : " + ptn )		
	    
	    val oRsrc : Option[emT.PlaceInstance] =
	      for(
		ltns <- labelToNS;
		ttv <- textToVar;
		ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
	      ) yield {
		ccl match {
		  case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
		    val oGvOrK = 
		      (if ( compareNameSpace( ns, kvNameSpace ) ) {	    
			// BUGBUG -- LGM need to return the Solution
			// Currently the PersistenceManifest has no access to the
			// unification machinery	      
			
			for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
			yield {				    
			  Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
			}
		      }
		       else {
			 if ( compareNameSpace( ns, kvKNameSpace ) ) {
			   Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
			 }
			 else {
			   throw new Exception( "unexpected namespace : (" + ns + ")" )
			 }
		       });

		    val cclKey =
		      xmlIfier.fromXML( ltns, ttv, ttt )(
			xmlIfier.asXML( key )
		      ) match {
			case Some( cclX ) => cclX
			case _ => throw new Exception( "xml roundtrip failed " + key )
		      }

		    val soln = 
		      unifyQuery(
			asPatternString(
			  cclKey
			),
			asPatternString( k )
		      )
		    emT.PlaceInstance(
		      k,
		      oGvOrK match {
			case Some( Left( gv ) ) => {
			  Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
			}
			case Some( Right( mTT.Continuation( ks ) ) ) => {
			  Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
			}
			case _ => {
			  throw new Exception( "excluded middle contract broken: " + oGvOrK )
			}
		      },
		      // BUGBUG -- lgm : why can't the compiler determine
		      // that this cast is not necessary?
		      theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
		    )
		  }
		  case _ => {
		    throw new Exception( "unexpected record format : " + value )
		  }
		}      
	      }
	    
	    // BUGBUG -- lgm : this is a job for flatMap
	    oRsrc match {
	      case Some( pI ) => {
		pI
	      }
	      case _ => {
		throw new Exception( "violated excluded middle : " + oRsrc )
	      }
	    }
	  }
 
	}
	
	override def asCacheK(
	  ccl : CnxnCtxtLabel[String,String,String]
	) : Option[mTT.Continuation] = {
	  tweet(
	    "converting to cache continuation stack" + ccl
	  )
	  ccl match {
	    case CnxnCtxtBranch(
	      "string",
	      CnxnCtxtLeaf( Left( rv ) ) :: Nil
	    ) => {
	      val unBlob =
		continuationStorageType match {
		  case "CnxnCtxtLabel" => {
		    // tweet(
// 		      "warning: CnxnCtxtLabel method is using XStream"
// 		    )
		    fromXQSafeJSONBlob( rv )
		  }
		  case "XStream" => {
		    fromXQSafeJSONBlob( rv )
		  }
		  case "Base64" => {
		    val data : Array[Byte] = Base64Coder.decode( rv )
		    val ois : ObjectInputStream =
		      new ObjectInputStream( new ByteArrayInputStream(  data ) )
		    val o : java.lang.Object = ois.readObject();
		    ois.close()
		    o
		  }
		}
	      
	      unBlob match {
		case k : mTT.Resource => {
		  Some( k.asInstanceOf[mTT.Continuation] )
		}
		case _ => {
		  throw new Exception(
		    (
		      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		      + "ill-formatted continuation stack blob : " + rv
		      + "\n" 
		      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		      + "\n"
		      + "unBlob : " + unBlob
		      + "\n"
		      + "unBlob type : " + unBlob
		      + "\n"
		      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		    )
		  )
		}
	      }
	    }
	    case _ => {
	      throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
	    }
	  }
	}
	
	override def asCacheK(
	  ltns : String => String,
	  ttv : String => String,
	  value : Elem
	) : Option[mTT.Continuation] = {
	  throw new Exception( "shouldn't be calling this version of asCacheK" )
	}	
	
	override def persistenceManifest : Option[PersistenceManifest] = {
	  val sid = Some( ( s : String ) => s )
          val kvdb = this;
	  Some(
	    new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid ) {
	      override def valueStorageType : String = {
		kvdb.valueStorageType
	      }
	      override def continuationStorageType : String = {
		kvdb.continuationStorageType
	      }
	    }
	  )
	}
	
      }
      
    }
    
    import Being._

    def singleton( storeUnitStr : String, a : String )  = {
      new PersistedStringMGJ( storeUnitStr, a, List( ) )
    }

    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      new PersistedStringMGJ( storeUnitStr, a, List( b ) )
    }

    def loopBack( storeUnitStr : String ) = {
      ptToPt( storeUnitStr, "localhost", "localhost" )
    }

    import scala.collection.immutable.IndexedSeq
        
    type MsgTypes = DTSMSH[String,String,String,String]   
    
    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    object MonadicDMsgs extends MsgTypes with Serializable {
      val aLabel =
	new CnxnCtxtLeaf[String,String,String](
	  Left(
	    "a"
	  )
	)
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
      override def protoJtsreq : JTSReq =
	JustifiedRequest(
	  protoDreqUUID,
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDreq,
	  None
	)
      override def protoJtsrsp : JTSRsp = 
	JustifiedResponse(
	  protoDreqUUID,
	  new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDrsp,
	  None
	)
      override def protoJtsreqorrsp : JTSReqOrRsp =
	Left( protoJtsreq )
    }
    
    override def protoMsgs : MsgTypes = MonadicDMsgs            

  }
}

object KVDBJSONAPIDispatcher {
  def apply(
    srcHost : String,
    srcExchange : String
  ) : KVDBJSONAPIDispatcher = {
    new KVDBJSONAPIDispatcher( srcHost, srcExchange )
  }
  def unapply(
    kvdbDispatcher : KVDBJSONAPIDispatcher
  ) : Option[( String, String )] = {
    Some( ( kvdbDispatcher.srcHost, kvdbDispatcher.srcExchange ) )
  }
}
