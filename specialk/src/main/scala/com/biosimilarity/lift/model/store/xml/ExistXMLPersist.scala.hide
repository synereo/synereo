// -*- mode: Scala;-*- 
// Filename:    ExistXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:44:09 2011 
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
import com.biosimilarity.lift.model.store.CnxnXQuery
import com.biosimilarity.lift.lib._

import org.exist.storage.DBBroker

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

import javax.xml.transform.OutputKeys
import java.util.UUID

object ExistDefaults
extends XMLStoreDefaults {
  implicit val URI : String  =
    "xmldb:exist://localhost:8080/exist/xmlrpc"
  implicit val driver : String =
    "org.exist.xmldb.DatabaseImpl"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
  val queryServiceType : String = "XQueryService"
  val queryServiceVersion : String = "1.0"
  val managementServiceType : String =
    "CollectionManagementService"
  val managementServiceVersion : String = "1.0"  
}

trait ExistXMLStore extends XMLStore {
  self : UUIDOps =>

    //override type ConfigurationDefaults = ExistDefaults.getClass

  override def configurationDefaults : ConfigurationDefaults = {
    ExistDefaults.asInstanceOf[ConfigurationDefaults]
  }
}

class SimpleCnxnStore[Namespace,Var,Tag] 
extends ExistXMLStore
with CnxnStorage[Namespace,Var,Tag] 
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnXML[Namespace,Var,Tag]
with UUIDOps {
  override def tmpDirStr : String = CnxnStorageDefaults.tmpDirStr
}

class ExistRetrieveExample {
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
