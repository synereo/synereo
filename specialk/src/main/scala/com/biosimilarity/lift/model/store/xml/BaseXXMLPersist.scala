// -*- mode: Scala;-*- 
// Filename:    BaseXXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:45:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.CnxnLabel
import com.biosimilarity.lift.model.store.OntologicalStatus
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Hypothetical
import com.biosimilarity.lift.model.store.Theoretical
import com.biosimilarity.lift.model.store.CnxnLeaf
import com.biosimilarity.lift.model.store.CCnxnLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnBranch
import com.biosimilarity.lift.model.store.CnxnBranch
import com.biosimilarity.lift.model.store.CCnxnBranch
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
import com.biosimilarity.lift.model.store.CCnxnCtxtLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
import com.biosimilarity.lift.model.store.CCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.Cnxn
import com.biosimilarity.lift.model.store.CCnxn
import com.biosimilarity.lift.model.store.CnxnXML
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.CnxnXQuery
import com.biosimilarity.lift.lib._

import org.basex.api.xmldb.BXCollection
import org.basex.BaseXClient
import org.basex.server.ClientSession
import org.basex.core.BaseXException
import org.basex.core.Context
import org.basex.core.cmd.{ List => BXListx ,_ }
import org.basex.data.Result
import org.basex.data.XMLSerializer
import org.basex.query.QueryException
import org.basex.query.QueryProcessor
import org.basex.query.item.Item
import org.basex.query.iter.Iter

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

import scala.xml._

import javax.xml.transform.OutputKeys
import java.util.UUID
import java.io.File

trait BaseXXMLStore extends XMLStore {
  self : Journalist
	 with ConfiggyReporting
	 with ConfiguredJournal
         with ConfigurationTrampoline
	 with UUIDOps =>
  
  var _clientSession : Option[ClientSession] = None
  def clientSession : ClientSession = {
    _clientSession match {
      case Some( cs ) => cs
      case None => {
	val cs =
	  new ClientSession(
	    dbHost,
	    dbPort.toInt,
	    dbUser,
	    dbPwd
	  )
	_clientSession = Some( cs )
	cs
      }
    }
  }

  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  override def getCollection( createIfMissing : Boolean )(
    xmlCollStr : String
  ) : Option[Collection] = {
    try {
      // BUGBUG -- LGM the semantics of BXCollection a little
      // different than createIfMissing; and requires catching the
      // database not found exception
      Some( new BXCollection( xmlCollStr, true ) )
    } 
    catch {
      case e : XMLDBException => {
	Some( new BXCollection( xmlCollStr, false ) )
      }
      case _ => None
    }
  }

  override def createResource( xmlColl : Collection, xmlRsrcStr : String )
  : Option[XMLResource] = {
    val document =
      xmlColl.createResource(
	null, XMLResource.RESOURCE_TYPE
      ).asInstanceOf[XMLResource]

    val f : File = new File( xmlRsrcStr )

    if ( f.canRead() ) {
      document.setContent( f )
      xmlColl.storeResource( document )
      Some( document )
    }
    else {      
      //println( "cannot read file " + xmlRsrcStr )
      None
    }    
  }

  def executeInContext(
    xmlCollStr : String,
    qrys : List[String],
    ostrm : java.io.OutputStream
  ) : Unit = {
    val dbCtxt = new Context()
    try {      
      new Open( xmlCollStr ).execute( dbCtxt )

      for( qry <- qrys ) {
	val xqry = new XQuery( qry )
	xqry.execute( dbCtxt, ostrm )
      }           
    }
    catch {
      case e : BaseXException => {
	new CreateDB( xmlCollStr ).execute( dbCtxt )
	for( qry <- qrys ) {
	  val xqry = new XQuery( qry )
	  xqry.execute( dbCtxt, ostrm )
	}
      }
      case e : Exception => {	
	tweetTrace( e )
      }
    }
    finally {
      dbCtxt.close()
    }
  }

  def executeInContext(
    xmlCollStr : String,
    qry : String,
    ostrm : java.io.OutputStream
  ) : Unit = {
    executeInContext( xmlCollStr, List( qry ), ostrm )
  }

  def executeInSession(
    qry : String
  ) : List[Elem] = {
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    clientSession.setOutputStream( srvrRspStrm )
    clientSession.execute( new XQuery( qry ) )
    val rsltsStr = srvrRspStrm.toString( "UTF-8" )
    srvrRspStrm.close
    rsltsStr match {
      case "" => {
	Nil
      }
      case _ => {
	XML.loadString(
	  (
	    "<results>"
	    + rsltsStr
	    + "</results>"
	  )
	).child.toList.filter(
	  ( x : Node ) => x.isInstanceOf[Elem]
	).asInstanceOf[List[Elem]]
      }
    }    
  }

  def executeInSession(
    qrys : List[String]
  ) : List[Elem] = {
    qrys.flatMap( executeInSession )
  }

  def executeInSession(
    xmlCollStr : String,
    qrys : List[String],
    ostrm : java.io.OutputStream
  ) : Unit = {
    for( qry <- qrys ) {
      clientSession.setOutputStream( ostrm )
      clientSession.execute( new XQuery( qry ) )
    }
  }

  def executeInSession(
    xmlCollStr : String,
    qry : String,
    ostrm : java.io.OutputStream
  ) : Unit = {
    executeInSession( xmlCollStr, List( qry ), ostrm )
  }
}

trait BaseXCnxnStorage[Namespace,Var,Tag]
extends CnxnStorage[Namespace,Var,Tag] {
  self : BaseXXMLStore
        with Journalist
	with ConfiggyReporting
	with ConfiguredJournal
        with ConfigurationTrampoline
	with UUIDOps =>
    
    override def tmpDirStr : String = {
      throw new Exception( "don't use the filebased api" )
    }

  def checkIfDBExists(
    xmlCollStr : String,
    leaveOpen : Boolean
  ) : Boolean = {
    try {
      val srvrRspStrm =
	new java.io.ByteArrayOutputStream()
      clientSession.setOutputStream( srvrRspStrm )
      clientSession.execute( new Open( xmlCollStr ) )
      if ( ! leaveOpen ) {
	clientSession.execute( new Close() )
      }
      true
    }
    catch {
      case e : BaseXException => {
	false
      }
    }
  }
  
  override def store( xmlCollStr : String )(
    cnxn : CnxnCtxtLabel[Namespace,Var,String]
  ) : Unit = {   
    //val dbCtxt = new Context()
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {
      tweet( 
	"attempting to open collection: " + xmlCollStr
      )
      clientSession.setOutputStream( srvrRspStrm )
      clientSession.execute( new Open( xmlCollStr ) )
      
      tweet( 
	"collection " + xmlCollStr + " opened"
      )

      val insertTemplate =
	(
	  "insert node %NODE% into "
	  + "for $db in collection('%COLLNAME%')/records return $db"
	);

      val nodeStr = 
	xmlIfier.asXML( cnxn ).toString

      tweet( 
	"attempting to insert record into database doc in " + xmlCollStr
      )

      //println( "record : \n" + nodeStr )
      
      val insertQry = 
	insertTemplate.replace(
	  "%NODE%",
	  nodeStr
	).replace(
	  "%COLLNAME%",
	  xmlCollStr
	)

      //println( "insertion query : \n" + insertQry )
      
      try {	
	clientSession.execute( new XQuery( insertQry ) )
      }
      catch {
	case e : BaseXException => {
	  tweet( 
	    "insertion query failed " + insertQry
	  )
	  tweetTrace( e )
	}
      }
    }
    catch {
      case e : BaseXException => {
	tweet( 
	  "failed to open " + xmlCollStr
	)
	tweet( 
	  "attempting to create " + xmlCollStr
	)
	clientSession.execute( new CreateDB( xmlCollStr ) )
	val recordElem = xmlIfier.asXML( cnxn )
	val recordsElem =
	  <records>{recordElem}</records>
	tweet( 
	  "adding database doc to " + xmlCollStr
	)
	clientSession.execute(
	  new Add(
	    recordsElem.toString,
	    "database"
	  )
	)          
      }      
    }    
    finally {
      srvrRspStrm.close()
    }
  }

  def keyValue(
    cnxn : CnxnCtxtLabel[Namespace,Var,String]
  ) : ( Node, Node ) = {
    val cxml = xmlIfier.asXML( cnxn )
    cxml.child.toList match {
      case k :: v :: Nil => {
	( k, v )
      }
      case _ => {
	throw new Exception( "malformed record: " + cxml )
      }
    }
  }

  override def update( xmlCollStr : String )(
    cnxn : CnxnCtxtLabel[Namespace,Var,String]
  ) : Unit = {   
    //val dbCtxt = new Context()
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {
      tweet( 
	"attempting to open collection: " + xmlCollStr
      )
      clientSession.setOutputStream( srvrRspStrm )
      clientSession.execute( new Open( xmlCollStr ) )
      
      tweet( 
	"collection " + xmlCollStr + " opened"
      )

      val replaceTemplate =
	(
	  "let $rcrds := "
	  + "("
	  + "for $rcrd in "
	  + "collection('%COLLNAME%')/database/records/record "
	  + "where $rcrd/*[1] = %KEY% "
	  + "return $rcrd"
	  + ")"
	  + "if ( count( $rcrds ) > 0 ) "
	  + "then "
	  + "return replace value of node $rcrds[1]/*[2]"
	  + "with %VALUE% "
	  + "else insert node %NODE% into "
	  + "collection('%COLLNAME%')/database/records "
	);

      val nodeStr = 
	xmlIfier.asXML( cnxn ).toString

      val ( k, v ) = keyValue( cnxn )

      tweet( 
	"attempting to update record in database doc in " + xmlCollStr
      )

      //println( "record : \n" + nodeStr )
      
      val replaceQry = 
	replaceTemplate.replace(
	  "%KEY%",
	  k.toString
	).replace(
	  "%VALUE%",
	  v.toString
	).replace(
	  "%NODE%",
	  nodeStr
	).replace(
	  "%COLLNAME%",
	  xmlCollStr
	)

      //println( "insertion query : \n" + insertQry )
      
      try {	
	clientSession.execute( new XQuery( replaceQry ) )
      }
      catch {
	case e : BaseXException => {
	  tweet( 
	    "insertion query failed " + replaceQry
	  )
	  tweetTrace( e )
	}
      }
    }
    catch {
      case e : BaseXException => {
	tweet( 
	  "failed to open " + xmlCollStr
	)
	tweet( 
	  "attempting to create " + xmlCollStr
	)
	clientSession.execute( new CreateDB( xmlCollStr ) )
	val recordElem = xmlIfier.asXML( cnxn )
	val recordsElem =
	  <records>{recordElem}</records>
	tweet( 
	  "adding database doc to " + xmlCollStr
	)
	clientSession.execute(
	  new Add(
	    recordsElem.toString,
	    "database"
	  )
	)          
      }      
    }    
    finally {
      srvrRspStrm.close()
    }
  }
}

class BaseXRetrieveExample
extends Journalist
with ConfiggyReporting
with ConfiggyJournal
with UUIDOps {
  def get(
    xmlColl : String,
    xmlRsrc : String,
    xmlPath : String
  ) : Unit = {
    val context : Context = new Context();
 
    reportage( "=== QueryCollection ===" )
 
    // ------------------------------------------------------------------------
    // Create a collection from all XML documents in the 'etc' directory
    reportage( "\n* Create a collection." )
 
    new CreateDB( xmlColl, xmlPath ).execute( context )
 
    // ------------------------------------------------------------------------
    // List all documents in the database
    reportage( "\n* List all documents in the database:" )
 
    // The XQuery base-uri() function returns a file path
    reportage(
      new XQuery(
        "for $doc in collection('" + xmlColl + "')" +
        "return <doc path='{ base-uri($doc) }'/>"
      ).execute( context )
    )
 
    // ------------------------------------------------------------------------
    // Evaluate a query on a single document
    reportage( "\n* Evaluate a query on a single document:" )
 
    // If the name of the database is omitted in the collection() function,
    // the currently opened database will be referenced
    reportage(
      new XQuery(
        "for $doc in collection()" +
        "let $file-path := base-uri($doc)" +
        "where ends-with($file-path, '" + xmlRsrc + "')" +
        "return concat($file-path, ' has ', count($doc//*), ' elements')"
      ).execute( context )
    )
 
    // ------------------------------------------------------------------------
    // Drop the database
    reportage( "\n* Drop the database." )
 
    new DropDB( xmlColl ).execute( context )
 
    // ------------------------------------------------------------------------
    // Close the database context
    context.close()
  }
}
