// -*- mode: Scala;-*- 
// Filename:    BaseXXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:45:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import org.basex.api.xmldb.BXCollection

import org.exist.storage.DBBroker

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

import javax.xml.transform.OutputKeys
import java.util.UUID

object BaseXDefaults {
  implicit val URI : String  =
    "http://localhost:8984/basex/jax-rx"
  implicit val driver : String =
    "org.basex.api.xmldb.BXDatabase"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
}

class BaseXXMLStore extends XMLStore {
  self : UUIDOps =>

  override def URI : String = BaseXDefaults.URI
  override def driver : String = BaseXDefaults.driver
  override def dbRoot : String = BaseXDefaults.dbRoot
  override def createDB : Boolean = BaseXDefaults.createDB
  override def indent : Boolean = BaseXDefaults.indent
  override def resourceType : String = BaseXDefaults.resourceType  
}
