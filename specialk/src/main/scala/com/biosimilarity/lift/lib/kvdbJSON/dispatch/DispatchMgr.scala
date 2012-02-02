// -*- mode: Scala;-*- 
// Filename:    DispatchMgr.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 30 23:13:05 2012 
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

trait DispatcherManagementProtocol {
  trait DispatcherManagementMessage
  case class AddSingletonKVDB(
    uri : URI, db : String, host : String
  ) extends DispatcherManagementMessage
  case class AddSingletonKVDBFromURI(
    uri : URI
  ) extends DispatcherManagementMessage
  case class AddPtToPtKVDB(
    srcURI : URI, srcHost : String,
    trgtURI : URI, trgtHost : String,
    db : String
  ) extends DispatcherManagementMessage
  case class AddPtToPtKVDBFromURIs(
    srcURI : URI, trgtURI : URI
  ) extends DispatcherManagementMessage
  case class AddReplyQueue(
    uri : URI, host : String, exchange : String
  ) extends DispatcherManagementMessage
}

class MgtMsgHandler(
  kvdbDispatcher : KVDBJSONAPIDispatcher,
  kvdbMgrURI : URI
) extends DispatcherManagementProtocol
with CnxnXQuery[String,String,String]
with CnxnCtxtInjector[String,String,String]
with UUIDOps
with Blobify
with CnxnXML[String,String,String]
with Journalist
with ConfiggyReporting
with ConfiguredJournal
with ConfigurationTrampoline {
  implicit def toPattern( s : String ) : CnxnCtxtLabel[String,String,String] = {
     fromCaseClassInstanceString( s )
   .getOrElse( null )
   .asInstanceOf[CnxnCtxtLabel[String,String,String]]
  }

  // Configuration
  override def configFileName : Option[String] = None
  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  def requestVar : Option[String] = None
  def requestFrame : Option[String] = None
  def requestPattern() : String = {
    (
      requestFrame.getOrElse( "managementRequest" )
      + "( "
      + requestVar.getOrElse( "Request" )
      + " )"
    )
  }

  def parseRequest(
    subst : org.prolog4j.Solution[String]
  ) : Option[DispatcherManagementMessage] = {
    tweet( "parsing request using substitution: " + subst )
    val reqV = requestVar.getOrElse( "Request" )
    val tSubst : org.prolog4j.Solution[org.prolog4j.Compound] =
      subst.on( reqV )
    val reqTerm = tSubst.get()
    val reqCCL = asPattern( reqTerm + "" )

    tweet( "the request term is: " + reqTerm )

    reqTerm.getFunctor match {
      case "addSingletonKVDB" => {
	Some(
	  AddSingletonKVDB(
	    new URI( reqTerm.getArg( 0 ) + "" ),
	    reqTerm.getArg( 1 ) + "",
	    reqTerm.getArg( 2 ) + ""
	  )
	)
      }
      case "addSingletonKVDBFromURI" => {
	Some(
	  AddSingletonKVDBFromURI(
	    new URI( reqTerm.getArg( 0 ) + "" )
	  )
	)
      }
      case "addPtToPtKVDB" => {
	Some(
	  AddPtToPtKVDB(
	    new URI( reqTerm.getArg( 0 ) + "" ),
	    reqTerm.getArg( 1 ) + "",
	    new URI( reqTerm.getArg( 2 ) + "" ),
	    reqTerm.getArg( 3 ) + "",
	    reqTerm.getArg( 4 ) + ""
	  )
	)
      }
      case "addPtToPtKVDBFromURIs" => {
	Some(
	  AddPtToPtKVDBFromURIs(
	    new URI( reqTerm.getArg( 0 ) + "" ),
	    new URI( reqTerm.getArg( 1 ) + "" )
	  )
	)
      }
      case "addReplyQueue" => {
	Some(
	  AddReplyQueue(
	    new URI( reqTerm.getArg( 0 ) + "" ),
	    reqTerm.getArg( 1 ) + "",
	    reqTerm.getArg( 2 ) + ""
	  )
	)
      }
      case _ => {
	None
      }
    }
  }

  def handleMgtMsg(
    kvdbDispatcher : KVDBJSONAPIDispatcher,
    kvdbMgrURI : URI
  ) : Unit = {        
    for( kvdbMgr <- kvdbDispatcher.namespace.get( kvdbMgrURI ) ) {
      reset {
	for( msg <- kvdbMgr.get( requestPattern() ) ) {
	  msg match {
	    case Some( kvdbDispatcher.stblKVDBScope.mTT.RBound( v, Some( subst ) ) ) => {	      
	       for( mgtReq <- parseRequest( subst ) ) {
		 mgtReq match {
		   case AddSingletonKVDB( uri, db, host ) => {
		     tweet( 
		       (
			 "adding singleton kvdb with storage " + db
			 + " running on " + host
			 + " mapped to " + uri
		       )
		     )
		     kvdbDispatcher.addSingletonKVDB( uri, db, host )
		   }
		   case AddSingletonKVDBFromURI( uri ) => {
		     tweet( 
		       (
			 "adding singleton kvdb mapped to " + uri
		       )
		     )
		     kvdbDispatcher.addSingletonKVDB( uri )
		   }
		   case AddPtToPtKVDB(
		     srcURI, srcHost, trgtURI, trgtHost, db
		   ) => {
		     tweet( 
		       (
			 "adding ptToPt kvdb with storage " + db
			 + " with src host " + srcHost
			 + " mapped to " + srcURI
			 + " and trgt host " + trgtHost
			 + " mapped to " + trgtURI
		       )
		     )
		     kvdbDispatcher.addPtToPtKVDB(
		       srcURI, srcHost, trgtURI, trgtHost, db
		     )
		   }
		   case AddPtToPtKVDBFromURIs(
		     srcURI, trgtURI
		   ) => {
		     tweet( 
		       (
			 "adding ptToPt kvdb "
			 + "with srcURI " + srcURI
			 + " and trgtURI " + trgtURI
		       )
		     )
		     kvdbDispatcher.addPtToPtKVDB(
		       srcURI, trgtURI
		     )
		   }
		   case AddReplyQueue(
		     uri, host, exchange
		   ) => {
		     tweet( 
		       (
			 "adding reply queue on exchange " + exchange
			 + " running on " + host
			 + " mapped to " + uri
		       )
		     )
		     kvdbDispatcher.addReplyQueue( uri, host, exchange )
		   }
		 }
	       }	    
	    }
	    case _ => {
	      throw new Exception( "bad management request: " + msg )
	    }
	  }
	}
      }
    }
  }
}

package usage {
  trait StandardDispatchController {
    def dispatcher : KVDBJSONAPIDispatcher
    def trgtURI : URI
    def srcURI : URI
    def replyURI : URI
    def setup(
      dispatcher : KVDBJSONAPIDispatcher,
      trgtURI : URI,
      srcURI : URI,
      replyURI : URI
    ) : KVDBJSONAPIDispatcher
    def setup() : KVDBJSONAPIDispatcher = 
      setup( dispatcher, trgtURI, srcURI, replyURI )
  }

  case class StdDispatchController(
    override val dispatcher : KVDBJSONAPIDispatcher,
    override val trgtURI : URI,
    override val srcURI : URI,
    override val replyURI : URI
  ) extends StandardDispatchController {
    def setup(
      dispatcher : KVDBJSONAPIDispatcher,
      trgtURI : URI,
      srcURI : URI,
      replyURI : URI
    ) : KVDBJSONAPIDispatcher = {
      // Configure dispatcher to serve requests with a "to" header of trgtURI
      // and a "from" header of srcURI
      // and deposit replies according to the following scheme
      // if replyURI = scheme://host/root
      // replyQueue = root_queue, replyExchange = root_exchange
      
      dispatcher.addSingletonKVDB( trgtURI )
      dispatcher.addReplyQueue( srcURI, replyURI.getHost, replyURI.getPath.split( "/" )( 1 ) )
      dispatcher.serveAPI
      
      dispatcher
    }
  }

  object Dispatcher extends StdDispatchController(
    new KVDBJSONAPIDispatcher( "localhost", "kvdb" ),
    new URI( "agent", "localhost", "/kvdbDispatchStore1", "" ),
    new URI( "agent", "localhost", "/kvdbDispatchStore2", "" ),
    new URI( "agent", "localhost", "/kvdbReply", "" )
  ) {    
  }
}
