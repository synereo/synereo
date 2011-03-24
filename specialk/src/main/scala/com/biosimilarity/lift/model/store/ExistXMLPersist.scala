// -*- mode: Scala;-*- 
// Filename:    ExistXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:44:09 2011 
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
