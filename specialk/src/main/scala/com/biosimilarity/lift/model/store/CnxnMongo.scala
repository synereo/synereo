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

  def toMongoQuery(
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
      case ccb@CnxnCtxtBranch( functor, factuals ) => {
	def loop( ccl : CnxnCtxtLabel[Namespace,Var,Tag] ) : List[String] = {
	  ccl match {
	    case CnxnCtxtLeaf( Left( tag ) ) => {
	      List( ( tag + "" ) )
	    }
	    case CnxnCtxtLeaf( Right( "_" ) ) => {
	      // BUGBUG : lgm -- incorrect semantics
	      List( "" )
	    }
	    case CnxnCtxtLeaf( Right( v ) ) => {
	      // BUGBUG : lgm -- incorrect semantics
	      List( ( v + "" ) )
	    }
	    case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
	      ( List[String]() /: innerFactuals )(
		{
		  ( acc, e ) => {
		    (
		      acc
		      ++
		      (
			loop( e ).map(
			  {
			    ( path ) => {
			      path match {
				case "" => innerFunctor + ""
				case _ => innerFunctor + "." + path
			      }
			    }
			  }
			)
		      )
		    )
		  }
		}
	      )
	    }
	  }
	}

	( MongoDBObject.empty /: loop( ccb ) )(
	  {
	    ( acc, e ) => {
	      acc ++ ( e $exists true )
	    }
	  }
	)

      }
    }
  }  
}

trait CnxnMongoObject[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag]
         with CnxnString[Namespace,Var,Tag]
	 with Blobify with UUIDOps =>

  implicit def toMongoObject(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )  : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	MongoDBObject( ( tag + "" ) -> 1 )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	MongoDBObject( ( v + "" ) -> 1 )
      }
      case CnxnCtxtBranch( functor, factuals ) => {
	MongoDBObject( ( functor + "" ) -> toMongoObjectInner( cnxn ) )
      }
    }
  }

  def toMongoObjectInner(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	MongoDBObject( ( tag + "" ) -> 1 )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	MongoDBObject( ( v + "" ) -> 1 )
      }
      case CnxnCtxtBranch( functor, factuals ) => {
	val functorStr = functor + ""	
	val factArray = new Array[Any]( factuals.length )
	
	MongoDBObject(
	  for( fact <- factuals ) yield {
	    fact match {
	      case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
		( innerFunctor + "" ) -> toMongoObjectInner( fact )
	      }
	      case CnxnCtxtLeaf( Left( tag ) ) => {
		( tag + "" ) -> 1
	      }
	      case CnxnCtxtLeaf( Right( v ) ) => {
		// BUGBUG : lgm -- incorrect semantics
		( v + "" ) -> 1
	      }
	    }
	  }
	)
      }
    }
  }  
}

package usage {
  object MyCnxnMongoQuery extends CnxnMongoQuery[String,String,String]
    with CnxnMongoObject[String,String,String]
    with CnxnCtxtInjector[String,String,String]
    with CnxnString[String,String,String]
    with Blobify with UUIDOps {
      implicit def fromString( s : String ) : CnxnCtxtLabel[String,String,String] = {
	fromCaseClassInstanceString( s ) match {
	  case Some( ccl ) => ccl.asInstanceOf[CnxnCtxtLabel[String,String,String]];
	  case None => throw new Exception( "failed to parse" )
	}
      }
    }
}
