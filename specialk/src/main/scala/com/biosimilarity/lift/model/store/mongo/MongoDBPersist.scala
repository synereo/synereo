// -*- mode: Scala;-*- 
// Filename:    MongoXMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 24 10:45:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.mongo

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.CnxnLabel
import com.biosimilarity.lift.model.store.OntologicalStatus
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Hypothetical
import com.biosimilarity.lift.model.store.Theoretical
import com.biosimilarity.lift.model.store.CnxnLeaf
//import com.biosimilarity.lift.model.store.CCnxnLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnBranch
import com.biosimilarity.lift.model.store.CnxnBranch
//import com.biosimilarity.lift.model.store.CCnxnBranch
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
//import com.biosimilarity.lift.model.store.CCnxnCtxtLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
//import com.biosimilarity.lift.model.store.CCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.Cnxn
import com.biosimilarity.lift.model.store.CCnxn
import com.biosimilarity.lift.model.store.CnxnMongoObject
import com.biosimilarity.lift.model.store.CnxnMongoQuery
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.JSONIfy
import com.biosimilarity.lift.lib._

import com.mongodb.casbah.Imports._
//import com.mongodb.util.JSON
//import com.mongodb.util.JSON._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import javax.xml.transform.OutputKeys
import java.util.UUID
import java.io.File

trait MongoDBStore[Namespace,Var,Tag] extends BaseMongoPersist[Namespace,Var,Tag] {
  trait MongoRecordContents {
    def key : String
    def value : String 
  }
  object MongoRecordContents {
    def unapply(
      mrcrd : MongoRecordContents
    ) : Option[( String, String )] = {
      Some( ( mrcrd.key, mrcrd.value ) )
    }
  }
  case class MongoRecord(
    record : Record
  )
  case class MongoKRecord(
    krecord : KRecord
  )
  case class Record(
    val key   : String,
    val value : String
  ) extends MongoRecordContents
  case class KRecord(
    val key   : String,
    val value : String
  ) extends MongoRecordContents
}

trait MongoCnxnStorage[Namespace,Var,Tag]
       extends BaseMongoPersist[Namespace,Var,Tag]
       with JSONIfy[Namespace,Var,Tag] {
  self : MongoDBStore[Namespace,Var,Tag] with UUIDOps =>          
  @transient
  implicit val formats = Serialization.formats(NoTypeHints)
  def storeX( xmlCollStr : String )(
    cnxn: CnxnCtxtLabel[Namespace, Var, Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : Unit =
  {
    val ( rcrdT, k, v ) =
      keyValue( cnxn )(
	nameSpaceToString,
	varToString,
	tagToString
      )
    insertUpdate( rcrdT )( xmlCollStr, k.toString, v.toString )
  }
  def store( xmlCollStr : String )(
    cnxn: CnxnCtxtLabel[Namespace, Var, Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : Unit =
  {    
    insertUpdateRecord(
      CnxnMongoObjectifier.toMongoObject( cnxn )(
	nameSpaceToString, varToString, tagToString
      ),
      xmlCollStr
    )
  }

  def keyValue(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : ( String, String, String ) = {
    val cjsonStr =
      CnxnMongoObjectifier.toMongoObject( cnxn )(
	nameSpaceToString, varToString, tagToString
      ).toString
    val cjsonAST = parse( cjsonStr )
    //println( "cjsonStr: " + cjsonStr )
    //println( "cjsonStrAST: " + cjsonAST )
    val kvTpl =
      for {
	JObject( child ) <- cjsonAST;
	JField( "record", JObject( contents ) ) <- child;
	JField( "key", k ) <- contents;
	JField( "value", v ) <- contents
      } yield ( k, v );
    kvTpl match {
      case Nil => {
	val kkTpl = for {
	  JObject( child ) <- cjsonAST;
	  JField( "krecord", JObject( contents ) ) <- child;
	  JField( "key", k ) <- contents;
	  JField( "value", v ) <- contents
	} yield ( k, v );
	kkTpl match {
	  case ( k, ktn ) :: r => ( "krecord", compact( k ), compact( ktn ) )
	  case Nil => {
	    throw new Exception( "malformed record: " + cnxn )
	  }
	}
      }
      case ( k, v ) :: r => ( "record", compact( k ), compact( v ) )
    }
      
  } 

 def keyValueJSON(
    record : DBObject
  ) : Option[( String, JValue, JValue )] = {
   // BUGBUG -- lgm : If we can query the record directly via Casbah,
   // then we don't have to take the two unparsing and reparsing hit
   val JObject( child ) = parse( record.toString )
   val rcrdCntnts =
     for(
       JField( kvNameSpace, JObject( contents ) ) <- child		    
     ) yield contents

   val kvTpl =
     rcrdCntnts match {
       case Nil => {
	 for {
	   JField( kvKNameSpace, JObject( kcontents ) ) <- child
	   JField( "key", k ) <- kcontents;
	   JField( "value", v ) <- kcontents
	 } yield ( "krecord", k, v )
       }
       case fields => {
	 for {
	   JField( "key", k ) <- fields;
	   JField( "value", v ) <- fields
	 } yield ( "record", k, v )
       }
     }

   kvTpl match {
     case Nil => {
       None
     }
     case ( recordType, k, v ) :: _ => Some( ( recordType, k, v ) )
   }
 }

 def keyValue(
    record : DBObject
  ) : Option[( String, String, String )] = {
   // BUGBUG -- lgm : If we can query the record directly via Casbah,
   // then we don't have to take the two unparsing and reparsing hit
   for( ( rcrdType, k, v ) <- keyValueJSON( record ) )
   yield {
     ( rcrdType, compact( k ), compact( v ) )
   }
 }

 def delete(
   xmlCollStr : String, path : CnxnCtxtLabel[Namespace, Var, Tag]
 )(
   implicit nameSpaceToString : Namespace => String,
   varToString : Var => String,
   tagToString : Tag => String,
   ltns : String => Namespace
  ) : Unit =
  {
    deleteData( xmlCollStr, path )( nameSpaceToString, varToString, tagToString, ltns )
    deleteContinuation( xmlCollStr, path )( nameSpaceToString, varToString, tagToString, ltns )
  }

 def deleteData(
   xmlCollStr : String, path : CnxnCtxtLabel[Namespace, Var, Tag]
 )(
   implicit nameSpaceToString : Namespace => String,
   varToString : Var => String,
   tagToString : Tag => String,
   ltns : String => Namespace
  ) : Unit =
  {
    val keyQry =
      CnxnMongoQuerifier.toMongoQuery(
	new CnxnCtxtBranch[Namespace,Var,Tag](
	  ltns( "record" ),
	  List(
	    new CnxnCtxtBranch[Namespace,Var,Tag](
	      ltns( "key" ),
	      List(
		path.asInstanceOf[CnxnCtxtLabel[Namespace, Var, Tag] with Factual]
	      )
	    )
	  )
	)
      )(
	nameSpaceToString, varToString, tagToString
      )

    // BUGBUG -- lgm : should get record types from persistence manifest
    deleteRecordsMatchingKey( keyQry )( xmlCollStr )
  }
 def deleteContinuation(
   xmlCollStr : String, path : CnxnCtxtLabel[Namespace, Var, Tag]
 )(
   implicit nameSpaceToString : Namespace => String,
   varToString : Var => String,
   tagToString : Tag => String,
   ltns : String => Namespace
  ) : Unit =
  {
    val keyQry =
      CnxnMongoQuerifier.toMongoQuery(
	new CnxnCtxtBranch[Namespace,Var,Tag](
	  ltns( "kRecord" ),
	  List(
	    new CnxnCtxtBranch[Namespace,Var,Tag](
	      ltns( "key" ),
	      List(
		path.asInstanceOf[CnxnCtxtLabel[Namespace, Var, Tag] with Factual]
	      )
	    )
	  )
	)
      )(
	nameSpaceToString, varToString, tagToString
      )

    // BUGBUG -- lgm : should get record types from persistence manifest
    deleteRecordsMatchingKey( keyQry )( xmlCollStr )
  }
}

package usage {
  object MongoKVDBFeatureComponents
       extends MongoCnxnStorage[String,String,String]
       with JSONIfy[String,String,String] 
       with MongoDBStore[String,String,String] with UUIDOps {    
    object MyCCLConversions {
      implicit def toConverter(
	ccl : CnxnCtxtLabel[String,String,String]
      ) : CCLConversionsWrapper =
	CCLConversionsWrapper(
	  ccl, ( s : String ) => s, ( s : String ) => s, ( s : String ) => s
	)
      def apply(
	ccl : CnxnCtxtLabel[String,String,String]
      ) : CCLConversionsWrapper =
	CCLConversionsWrapper(
	  ccl, ( s : String ) => s, ( s : String ) => s, ( s : String ) => s
	)
    }
  }

  object JSONData {
    import MongoKVDBFeatureComponents._
    val cclRcrd1 : CnxnCtxtLabel[String,String,String] =
      CCLStringConversions(
	"record( key( t1( a( 1 ), b( \"a string is born\" ), c( true ) ) ), value( \"stuff\" ) )"
      ).toCCL()
    val cclKRcrd1 : CnxnCtxtLabel[String,String,String] =
      CCLStringConversions(
	"krecord( key( t1( a( 1 ), b( \"a string is born\" ), c( true ) ) ), value( \"<ASerializedClosure>\" ) )"
      ).toCCL()    
  }

}
