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

import com.biosimilarity.lift.lib.http._

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store._
//import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import com.biosimilarity.lift.lib.http.LockFreeMap

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import scala.util.continuations._
import scala.collection.mutable.HashMap
import scala.xml._

import org.prolog4j._
import biz.source_code.base64Coder.Base64Coder

import org.eclipse.jetty.websocket.WebSocket

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
with KVDBJSONAPIProtocolFormatT
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
  def srcURI : URI 
  def srcHost( srcURI : URI ) : String = srcURI.getHost
  def srcHost : String = srcHost( srcURI )
  def srcExchange( srcURI : URI ) : String =
    srcURI.getPath.split( "/" )( 1 )
  def srcExchange : String = srcExchange( srcURI )

  def srcScope : AMQPNodeJSStdScope =
    new AMQPNodeJSStdScope()
  @transient lazy val stblSrcScope : AMQPNodeJSStdScope = srcScope
  def srcQM( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPNodeJSQueueM =
    new stblSrcScope.AMQPNodeJSQueueM( srcHost, srcExchange )
  def srcQM : stblSrcScope.AMQPNodeJSQueueM =
    srcQM( srcHost, srcExchange )
  @transient lazy val stblSrcQM : stblSrcScope.AMQPNodeJSQueueM = srcQM
  def srcQ( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPQueue[String] = 
    srcQM( srcHost, srcExchange ).zeroJSON
  def srcQ : stblSrcScope.AMQPQueue[String] = stblSrcQM.zeroJSON

  def replyScope : AMQPNodeJSStdScope =
    new AMQPNodeJSStdScope()
  @transient lazy val stblReplyScope : AMQPNodeJSStdScope = replyScope
  def replyQM( replyHost : String, replyExchange : String ) : stblReplyScope.AMQPNodeJSQueueM =
    new stblReplyScope.AMQPNodeJSQueueM( replyHost, replyExchange )
  def replyQ( replyHost : String, replyExchange : String ) : stblReplyScope.AMQPQueue[String] = 
    replyQM( replyHost, replyExchange ).zeroJSON

  def socketURIMap: LockFreeMap[URI,SocketConnectionPair]
  
  // namespace
  def kvdbScope : PersistedTermStoreScope[String,String,String,String]
  lazy val stblKVDBScope : PersistedTermStoreScope[String,String,String,String] =
    kvdbScope
  def kvdbPersistenceScope : stblKVDBScope.PersistenceScope
  lazy val stblKVDBPersistenceScope : stblKVDBScope.PersistenceScope =
    kvdbPersistenceScope

  // this controls what URI's will be dispatched to KVDB's
  def namespace : HashMap[URI,stblKVDBPersistenceScope.PersistedMonadicGeneratorJunction]

  type ReplyTrgt = Either[URI,URI]
  trait ReplyCacheTrgt
  case class AMQPTrgt(
    monad : stblReplyScope.AMQPNodeJSQueueM,
    queue : stblReplyScope.AMQPQueue[String]
  ) extends ReplyCacheTrgt
  case class JVMWriter(
    writer : java.io.Writer
  ) extends ReplyCacheTrgt
  case class WebSocketTrgt(
    wsConnection : WebSocket.Connection
  ) extends ReplyCacheTrgt

  // this controls what URI's will be dispatched to with results
  // Note bene: the URI format is expected to tell us what kind of
  // reply cache target is to be used:
  //
  // amqp://host/trgtLocation
  // writer://fullyQualifiedWriterClass
  //

  def replyNamespace : HashMap[URI,ReplyTrgt]
  def replyNamespaceCache : HashMap[URI,ReplyCacheTrgt]

  // service
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
	  case grndRsrc : stblKVDBScope.mTT.Ground => {
	    Some( asJSONPairs( subst, ptrnVars ) )
	  }
	}
      }
      case stblKVDBScope.mTT.RBound( None, Some( subst ) ) => {
	Some( asJSONPairs( subst, ptrnVars ) )
      }
      case stblKVDBScope.mTT.RBound( _, None ) => {
	None
      }
      case _ => {
	throw new Exception( "unhandled resource case while getting substitution: " + rsrc )
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
	tweet( "retrieved rsrc: " + rsrc )
	val reqPtrn = toJSON( pattern )	
	tweet( "for pattern: " + reqPtrn )
	val ( rspVal, rspSubst ) =
	  rsrc match {
	    case stblKVDBScope.mTT.Ground( v ) => {
	      tweet( "retrieved ground value: " + v )
	      ( v + "", "null" )
	    }
	    case bndRsrc : stblKVDBScope.mTT.RBound => {
	      tweet( "retrieved bound resource: " + bndRsrc )
	      (
		getGrndVal( bndRsrc ), 
		substAsJSONPairs( patternVars( pattern), bndRsrc ) match {
		  case Some( "" ) => {
		    "null"
		  }
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
	 + "\"answer\"" + " : "
	 + "[ "
	 + reqPtrn + ", "
	 + rspSubst + ", "
	 + (
	   rspVal match {
	     case Some( rval ) => {
	       "\"" + rval + "\""
	     }
	     case _ => "null"
	   }
	 )
	 + " ]"
	 + " }"      
       )
      }

    tweet( "response body: " + body )

    (
      "{ "
      + "\"headers\"" + " : "
      + headers.getOrElse( "null" ).replace( "#", "" )
      + ", "
      + "\"body\"" + " : "
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
  ) : Option[ReplyCacheTrgt] = {
    def cacheMnQ(
      trgt : URI, reply : URI
    ) : ReplyCacheTrgt = {  
      val replyTrgt : ReplyCacheTrgt = 
	reply.getScheme match {
	  case "amqp" => {
	    tweet( "using amqp reply target scheme " )
	    val ( replyHost, replyExchange ) =
	      ( reply.getHost, reply.getPath.split( "/" )( 1 ) );
	      
	    tweet( "make reply target queue " + replyExchange + "_queue" + "@" + replyHost )
	    val replyM = replyQM( replyHost, replyExchange )
	    val replyQ = replyM.zeroJSON
	    
	    AMQPTrgt( replyM, replyQ )
	    
	  }
	  
	  case "websocket" => {
	    socketURIMap.get( reply ) match {
	      case Some( scp ) => {
		WebSocketTrgt( scp.responseConnection )
	      }
	      case _ => {
		throw new Exception( "missing websocket for : " + reply )
	      }
	    }
	  }

	  // BUGBUG : lgm -- BTW, this is a major security hole!!!
	  // Actually, by moving the scheme to the reply URI, it's
	  // slightly less of a security hole...
	  case "writer" => {
	    tweet( "using writer reply target scheme" )
	    tweet( "loading writer class: " + trgt.getHost )
	    val cls =
	      Thread.currentThread.getContextClassLoader.loadClass( trgt.getHost )
	    
	    tweet( "creating and initializing writer instance" )
	    val writer : java.io.Writer =
	      cls.getConstructor().newInstance( trgt.getPath.split( "/" ) ).asInstanceOf[java.io.Writer]

	    JVMWriter( writer )
	  }
	}      

      replyNamespaceCache += ( ( trgt, replyTrgt ) )
      replyNamespace += ( ( trgt, Right[URI,URI]( reply ) ) )
      
      replyTrgt
      
    }

    for( trgt <- asURI( reqHdr.uri_2 ); rplyTrgt <- replyNamespace.get( trgt ) ) yield {
      rplyTrgt match {
	case Left( reply ) => {	cacheMnQ( trgt, reply )	}
	case Right( reply ) => {
	  replyNamespaceCache.get( trgt ) match {
	    case Some( replyTrgt ) => {
	      tweet( "reusing reply target " + replyTrgt )
	      replyTrgt
	    }
	    case _ => {
	      tweet( "reconstituting cache from storage " )
	      cacheMnQ( trgt, reply )
	    }
	  }
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
	req match {
	  case askReq : KVDBAskReq => {	      
	    val pattern = asPattern( askReq )
	    tweet( "we have an ask request " + pattern + " from " + kvdb )
	    reset {
	      for( rslt <- kvdb.get( pattern ) ) {
		val rsp = asResponse( kvdbReqHdr, pattern, rslt )
		tweet( "response: " + rsp )
		for( replyTrgt <- asReplyTrgt( kvdbReqHdr ) ) {
		  tweet( "replyTrgt: " + replyTrgt )	  
		  replyTrgt match {
		    case AMQPTrgt( m, q ) => {
		      tweet( "sending " + rsp + " to " + q )
		      q ! rsp
		    }
		    case WebSocketTrgt( wsConnection ) => {
		      wsConnection.sendMessage( rsp )
		    }
		    case JVMWriter( writer ) => {
		      writer.write( rsp )
		    }		    
		  }		  		  
		}		
	      }
	    }
	  }
	  case tellReq : KVDBTellReq => {
	    val pattern = asPattern( tellReq )
	    val value = asValue( tellReq )
	    tweet( "we have a tell request to put " + value + " at " + pattern + " in " + kvdb )
	    reset {
	      kvdb.put( pattern, value )
	      for( replyTrgt <- asReplyTrgt( kvdbReqHdr ) ) {
		val rsp = craftResponse( reqHdr, tellReq )
		tweet( "replyTrgt: " + replyTrgt )	  
		replyTrgt match {
		  case AMQPTrgt( m, q ) => {
		    tweet( "sending " + rsp + " to " + q )
		    q ! rsp
		  }
		  case WebSocketTrgt( wsConnection ) => {
		    wsConnection.sendMessage( rsp )
		  }
		  case JVMWriter( writer ) => {
		    writer.write( rsp )
		  }
		}
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

  def serveAPI( srcHost : String, srcExchange : String ) : Unit = {
    tweet( "serving api on " + srcHost + " from amqp exchange " + srcExchange )
    val apiSrcQM = srcQM( srcHost, srcExchange )
    val apiSrcQ = srcQ( srcHost, srcExchange )  
    
    for( msg <- apiSrcQM( apiSrcQ ) ) {
      tweet( "received: " + msg )
      dispatch( parse( msg ) )
    }
  }  

  def serveAPI( stream : java.io.InputStream ) : Unit = {
    val bSrc = scala.io.Source.createBufferedSource( stream )
    for( jsonLine <- bSrc.getLines ) {
      tweet( "received: " + jsonLine )
      dispatch( parse( jsonLine ) )
    }
  }

  def serveAPI( queue : Iterator[String] ) : Unit = {
    println("entered serveAPI")
    for( jsonReq <- queue ) {
      println("received")
      tweet( "received: " + jsonReq )
      dispatch( parse( jsonReq ) )
    }
  }

  def serveAPI( uri : URI ) : Unit = {
    println("serveAPI " + uri)
    uri.getScheme match {
      case "amqp" => {
	serveAPI( srcHost( uri ), srcExchange( uri ) )
      }
      case "websocket" => {
        println("serveAPI websocket")
	for( scp <- socketURIMap.get( uri ) ) {
	  serveAPI( scp.requestQueue )
	}
      }
      case "stream" => {
	tweet( "using stream src scheme" )
	tweet( "loading stream class: " + uri.getHost )
	val cls =
	  Thread.currentThread.getContextClassLoader.loadClass( uri.getHost )
	
	tweet( "creating and initializing stream instance" )
	val stream : java.io.InputStream =
	  cls.newInstance( ).asInstanceOf[java.io.InputStream]
	
	serveAPI( stream )
      }      
    }
  }

  def serveAPI : Unit = serveAPI( srcURI ) 
}

class KVDBJSONAPIDispatcher(
  override val srcURI : URI
) extends KVDBJSONAPIDispatcherT {
  override def kvdbScope : PersistedTermStoreScope[String,String,String,String] = PTSS
  override def kvdbPersistenceScope : stblKVDBScope.PersistenceScope = {
    // This cast makes me sad...
    PTSS.Being.asInstanceOf[stblKVDBScope.PersistenceScope]
  }
  
  val socketURIMap = LockFreeMap[URI,SocketConnectionPair]()

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
  var stblReplyNamespace : Option[HashMap[URI,ReplyTrgt]] = None

  override def replyNamespaceCache : HashMap[URI,ReplyCacheTrgt] = {
    stblReplyNamespaceCache match {
      case Some( rnsc ) => rnsc
      case _ => {
	val rnsc = new HashMap[URI,ReplyCacheTrgt]()
	stblReplyNamespaceCache = Some( rnsc )
	rnsc
      }
    }
    
  }
  @transient var stblReplyNamespaceCache : Option[HashMap[URI,ReplyCacheTrgt]] = None

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
    val replyURI = new URI( "amqp", host, "/" + exchange, "" )
    replyNamespace +=
    ( ( uri, Left[URI,URI]( replyURI ) ) )
  }    

  def addReplyURI( uri : URI, replyURI : URI ) : Unit = {
    replyNamespace +=
    ( ( uri, Left[URI,URI]( replyURI ) ) )
  }    

  object PTSS
       extends PersistedTermStoreScope[String,String,String,String] 
       with UUIDOps
       with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    //object TheMTT extends MTTypes with Serializable
    @transient lazy val TheMTT : MTTypes with Serializable =
      new MTTypes with Serializable { }
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    //object TheDAT extends DATypes with Serializable
    @transient lazy val TheDAT : DATypes with Serializable =
      new DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT

    //object Being extends PersistenceScope with Serializable {      
    trait BeingScope extends PersistenceScope with Serializable {      
      
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
		  val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
		  val oos : ObjectOutputStream = new ObjectOutputStream( baos )
		  oos.writeObject( rsrc.asInstanceOf[Serializable] )
		  oos.close()
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

    @transient lazy val Being : BeingScope =
      new BeingScope { }
    
    //import Being._

    def singleton( storeUnitStr : String, a : String )  = {
      //new PersistedStringMGJ( storeUnitStr, a, List( ) )
      new Being.PersistedStringMGJ( storeUnitStr, a, List( ) )
    }

    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      //new PersistedStringMGJ( storeUnitStr, a, List( b ) )
      new Being.PersistedStringMGJ( storeUnitStr, a, List( b ) )
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
    srcURI : URI
  ) : KVDBJSONAPIDispatcher = {
    new KVDBJSONAPIDispatcher( srcURI )
  }
  def unapply(
    kvdbDispatcher : KVDBJSONAPIDispatcher
  ) : Option[( URI )] = {
    Some( ( kvdbDispatcher.srcURI ) )
  }
}


