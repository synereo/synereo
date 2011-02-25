// -*- mode: Scala;-*- 
// Filename:    XMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 02:03:57 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import org.exist.storage.DBBroker

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._
import javax.xml.transform.OutputKeys
import java.util.UUID

trait XMLStore {
  self : UUIDOps =>

  def URI : String
  def driver : String
  def dbRoot : String
  def createDB : Boolean
  def indent : Boolean
  def resourceType : String

  def driverClass : Class[_] = {
    Class.forName( driver )
  }

  def database : Database = {
    val db = driverClass.newInstance().asInstanceOf[Database]
    DatabaseManager.registerDatabase( db )
    if ( createDB ) {
      db.setProperty( "create-database", createDB.toString )
    }
    db
  }

  def getCollection( createIfMissing : Boolean )( xmlCollStr : String ) : Option[Collection] = {
    val xmlColl : Option[Collection] =          
      {
	val coll = DatabaseManager.getCollection( URI + xmlCollStr ) 
	if ( coll == null ) {
	  if ( createIfMissing ) {
	    val root : Collection = DatabaseManager.getCollection( URI + dbRoot )
	    val mgtService : CollectionManagementService =
	      root.getService("CollectionManagementService", "1.0").asInstanceOf[CollectionManagementService]

	    Some(
	      mgtService.createCollection(
		xmlCollStr.substring( "/db".length() )
	      )
	    )
	  }
	  else {
	    None
	  }
	}
	else {
	  Some( coll )
	}
      }

    for( xColl <- xmlColl ) {
      if ( indent ) {
	xColl.setProperty(OutputKeys.INDENT, "yes")
      } 
      else {
	xColl.setProperty(OutputKeys.INDENT, "no")
      }
    }

    xmlColl
  }

  def getResource( xmlColl : Collection )( xmlRsrcStr : String )
  : Option[XMLResource] = {
    val xmlRsrc =
      xmlColl.getResource( xmlRsrcStr ).asInstanceOf[XMLResource]
    if ( xmlRsrc == null ) {
      None
    }
    else {
      Some( xmlRsrc )
    }
  }

  def createResource( xmlColl : Collection )( xmlRsrcStr : String )
  : Option[XMLResource] = {
    import java.io.File

    val document : XMLResource =
      xmlColl.createResource( null, resourceType ).asInstanceOf[XMLResource]

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

  def getQueryService( xmlColl : Collection )(
    implicit qrySrvcType : String, qrySrvcVersion : String
  ) : Service = {
    val srvc = xmlColl.getService( qrySrvcType, qrySrvcVersion )
    if ( indent ) {
      srvc.setProperty( "indent", "yes" )
    }
    else {
      srvc.setProperty( "indent", "no" )
    }
    srvc
  }

  def execute( xmlColl : Collection )(
    srvc : Service
  )(
    qry : String
  ) : ResourceSet = {
    srvc match {
      case xqSrvc : XQueryService => {
	xqSrvc.execute( xqSrvc.compile( qry ) )
      }
      case _ => {
	throw new Exception( "execute not supported" )
      }
    }
  }

}

object ExistDefaults {
  implicit val URI : String  =
    "xmldb:exist://localhost:8080/exist/xmlrpc"
  implicit val driver : String =
    "org.exist.xmldb.DatabaseImpl"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
}

class ExistXMLStore extends XMLStore {
  self : UUIDOps =>

  override def URI : String = ExistDefaults.URI
  override def driver : String = ExistDefaults.driver
  override def dbRoot : String = ExistDefaults.dbRoot
  override def createDB : Boolean = ExistDefaults.createDB
  override def indent : Boolean = ExistDefaults.indent
  override def resourceType : String = ExistDefaults.resourceType  
}

trait CnxnStorage[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] with CnxnXML[Namespace,Var,Tag] with XMLStore with UUIDOps =>
    
    def tmpDirStr : String
  
  def store( xmlCollStr : String )( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter

    for( xmlColl <- getCollection( true )( xmlCollStr ) ) {
      val xmlRsrcId = getUUID()
      val xmlRsrcStr = tmpDirStr + "/" + xmlRsrcId.toString + ".xml"
      val fstream : FileWriter = new FileWriter( xmlRsrcStr )
      val out : BufferedWriter = new BufferedWriter(fstream)
      //out.write( toXML( cnxn ) ) -- Serialization-based storage
      out.write( asXML( cnxn ).toString ) // Data-binding-based storage
      out.close
    
      val xrsrc = createResource( xmlColl )( xmlRsrcStr )
    }
  }
}

object CnxnStorageDefaults {
  implicit val tmpDirStr = "tmp"
}

class SimpleCnxnStore[Namespace,Var,Tag] 
extends ExistXMLStore
with CnxnStorage[Namespace,Var,Tag] 
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnXML[Namespace,Var,Tag]
with UUIDOps {
  override def tmpDirStr : String = CnxnStorageDefaults.tmpDirStr
}

class RetrieveExample {
  val URI : String  = "xmldb:exist://localhost:8080/exist/xmlrpc"
  
  def get( xmlColl : String, xmlRsrc : String ) : Unit = {
    val driver : String = "org.exist.xmldb.DatabaseImpl"
    // initialize database driver
    val cl : Class[_] = Class.forName( driver )
    val database : Database = cl.newInstance().asInstanceOf[Database]
    DatabaseManager.registerDatabase( database )

    // get the collection
    val col : Collection =
      DatabaseManager.getCollection( URI + xmlColl )
    col.setProperty(OutputKeys.INDENT, "no")

    val res : XMLResource = col.getResource( xmlRsrc ).asInstanceOf[XMLResource]
    
    if (res == null) {
      println( "document not found!" )
    }
    else {
      println( res.getContent() )
    }
  }
}
