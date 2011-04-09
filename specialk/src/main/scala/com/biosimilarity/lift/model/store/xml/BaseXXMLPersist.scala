// -*- mode: Scala;-*- 
// Filename:    BaseXXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:45:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

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

import javax.xml.transform.OutputKeys
import java.util.UUID
import java.io.File

object BaseXDefaults {
  implicit val URI : String  =
    "xmldb:basex://localhost:1984/"
  val dbHost : String = "localhost"
  val dbPort : String = "1984"
  val dbUser : String = "admin"
  val dbPwd  : String = "admin"
  implicit val driver : String =
    "org.basex.api.xmldb.BXDatabase"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
  val queryServiceType : String = "XPathQueryService"
  val queryServiceVersion : String = "1.0"
  val managementServiceType : String =
    "CollectionManagementService"
  val managementServiceVersion : String = "1.0"  
  val valueStorageType : String = "CnxnCtxtLabel"
}

trait BaseXXMLStore extends XMLStore {
  self : UUIDOps with Journalist =>
  
    //override type ConfigurationDefaults = BaseXDefaults.getClass

  override def configurationDefaults : ConfigurationDefaults = {
    BaseXDefaults.asInstanceOf[ConfigurationDefaults]
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
      println( "cannot read file " + xmlRsrcStr )
      None
    }    
  }

  def executeInSession(
    qrys : List[String],
    ostrm : java.io.OutputStream
  ) : Unit = {
    try {
      val bxCtxt = new Context()
      for( qry <- qrys ) {
	val qproc = new QueryProcessor( qry, bxCtxt )
	val oostrm = new java.io.ObjectOutputStream( ostrm )
	val iter : Iter = qproc.iter()
	var nitem = iter.next
	while( nitem != null ) {
	  val item = nitem.toJava
	  if ( item.isInstanceOf[java.io.Serializable] ) {
	    oostrm.writeObject( item )
	  }
	  nitem = iter.next
	}
	qproc.close()
      }           
    }
    catch {
      case e : Exception => {	
	tweetTrace( e )
      }
    }    
  }

  def executeInSession(
    qry : String,
    ostrm : java.io.OutputStream
  ) : Unit = {
    executeInSession( List( qry ), ostrm )
  }
}

trait BaseXCnxnStorage[Namespace,Var,Tag]
extends CnxnStorage[Namespace,Var,Tag] {
  self : BaseXXMLStore with UUIDOps =>
    
    override def tmpDirStr : String = {
      throw new Exception( "don't use the filebased api" )
    }
  
  override def store( xmlCollStr : String )(
    cnxn : CnxnCtxtLabel[Namespace,Var,String]
  ) : Unit = {   
    for( xmlColl <- getCollection( true )( xmlCollStr ) ) {
      val document =
	xmlColl.createResource(
	  null, XMLResource.RESOURCE_TYPE
	).asInstanceOf[XMLResource]

      document.setContent( xmlIfier.asXML( cnxn ).toString )
      xmlColl.storeResource( document )
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
