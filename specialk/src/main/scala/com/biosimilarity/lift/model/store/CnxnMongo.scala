// -*- mode: Scala;-*- 
// Filename:    CnxnMongo.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb  6 09:24:30 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.zipper._

import scala.xml._
import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import com.mongodb.casbah.Imports._

trait CnxnMongoQuery[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag]
         with CnxnString[Namespace,Var,Tag]
	 with Blobify with UUIDOps =>

  implicit def toMongoQuery(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	( tag + "" ) $exists true
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	( v + "" ) $exists true
      }
      case CnxnCtxtBranch( functor, factuals ) => {
	val functorStr = functor + ""	
	val factArray = new Array[DBObject]( factuals.length )
	for( i <- ( 0 to factuals.length - 1 ) ) {
	  factArray( i ) = toMongoQuery( factuals( i ) )
	}
	MongoDBObject( functorStr -> MongoDBObject( "fields" -> factArray ) )
      }
    }
  }
}

package usage {
}
