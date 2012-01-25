// -*- mode: Scala;-*- 
// Filename:    Dispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 23 22:23:35 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.kvdbJSON

import com.biosimilarity.lift.lib.kvdbJSON.Absyn.{ URI => kvdbURI, UUID => kvdbUUID, _ }
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import scala.collection.mutable.HashMap

import org.prolog4j._

import java.net.URI
import java.io.StringReader

trait KVDBJSONAPIDispatcherT
extends Serializable
with CnxnXQuery[String,String,String]
with CnxnXML[String,String,String]
with CnxnCtxtInjector[String,String,String]
with Blobify
with UUIDOps {
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
    reqHdr match {
      case kvdbReqHdr : KVDBReqHdr => {
	for( ( m, q ) <- asReplyTrgt( kvdbReqHdr ) ) {
	  req match {
	    case askReq : KVDBAskReq => {
	      val pattern = asPattern( askReq )
	      reset {
		for( rslt <- kvdb.get( pattern ) ) {
		  q ! asResponse( kvdbReqHdr, pattern, rslt )
		}
	      }
	    }
	    case tellReq : KVDBTellReq => {
	      reset {
		kvdb.put( asPattern( tellReq ), asValue( tellReq ) )
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

  def dispatch( msg : Message ) : Unit = {
    msg match {
      case jreqHB : KVDBJustReqHB => {
	jreqHB.lblreqheader_ match {
	  case reqHdr : KVDBReqHdr => {
	    for( trgt <- asURI( reqHdr.uri_1 ); kvdb <- namespace.get( trgt ) ) {
	      dispatch( kvdb, reqHdr, getRequest( jreqHB ) )
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
	      dispatch( kvdb, reqHdr, getRequest( jreqBH ) )
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

class KVDBJSONAPIDispatcher(
  override val srcHost : String,
  override val srcExchange : String
) extends KVDBJSONAPIDispatcherT {
  override def kvdbScope : PersistedTermStoreScope[String,String,String,String] = {
    throw new Exception( "TODO : implement kvdbScope on " + this )
  }
  override def kvdbPersistenceScope : stblKVDBScope.PersistenceScope = {
    throw new Exception( "TODO : implement kvdbPersistenceScope on " + this )
  }
  // this controls what URI's will be dispatched to KVDB's
  override def namespace : HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]
  = {
    throw new Exception( "TODO : implement namespace on " + this )
  }

  // this controls what URI's will be dispatched to with results to db requests
  override def replyNamespace : HashMap[URI,ReplyTrgt] = {
    throw new Exception( "TODO : implement namespace on " + this )
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
