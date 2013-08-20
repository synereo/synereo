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

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

trait CnxnNSStringDefaults[Namespace,Var,Tag] {
  implicit val nameSpaceToString : Namespace => String =
    ( ns : Namespace ) => { ns + "" }
}
trait CnxnVarStringDefaults[Namespace,Var,Tag] {
  implicit val varToString : Var => String =
    ( v : Var ) => { v + "" }
}
trait CnxnTagStringDefaults[Namespace,Var,Tag] {
  implicit val tagToString : Tag => String =
    ( t : Tag ) => { t + "" }  
}

trait CnxnMongoSelectors {
  def tagSelector : String = "tag"
  def varSelector : String = "var"
}

trait CnxnStringDefaults[Namespace,Var,Tag] 
extends CnxnNSStringDefaults[Namespace,Var,Tag]
with CnxnVarStringDefaults[Namespace,Var,Tag] 
with CnxnTagStringDefaults[Namespace,Var,Tag]

trait CnxnNSVarSTagStringDefaults[Namespace,Var,Tag] {
  def ensureSafeField( s : String ) : String = {
    s.replace( ".", "&dot;" )
  }
  def recoverFieldName( s : String ) : String = {
    s.replace( "&dot;", "." )
  }
  val nameSpaceToString : Namespace => String =
    ( ns : Namespace ) => {
      ensureSafeField( ns + "" )
    }
  val varToString : Var => String =
    ( v : Var ) => {
      ensureSafeField( v + "" )
    }
  val tagToString : Tag => String =
    ( t : Tag ) => {
      ensureSafeField( t + "" )
    }  
}

trait CnxnMongoQuery[Namespace,Var,Tag]
extends CnxnMongoSelectors {
  self : CnxnCtxtInjector[Namespace,Var,Tag]
         with CnxnString[Namespace,Var,Tag] =>
	 /* with Blobify with UUIDOps */
  def paths( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : List[String] = {
    def extendPath( path : String, innerFunctor : String ) = {
      path match {
	case "" => innerFunctor + ""
	case _ => innerFunctor + "." + path
      }
    }
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	List( tagToString( tag ) )
      }      
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	//List( varToString( v ) )
	List( "" )
      }
      case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
	( List[String]() /: innerFactuals )(
	  {
	    ( acc, e ) => {
	      acc ++ paths(
		e
	      )( nameSpaceToString, varToString, tagToString ).map( extendPath( _, innerFunctor ) )
	    }
	  }
	)
      }
    }
  }

  def unifierPaths( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : List[String] = {
    def extendPath( path : String, innerFunctor : String ) = {
      path match {
	case "" => innerFunctor + ""
	case _ => innerFunctor + "." + path
      }
    }
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	List( "" )
      }      
      case CnxnCtxtLeaf( Right( v ) ) => {
	List( "" )
      }
      case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
	( List[String]() /: innerFactuals )(
	  {
	    ( acc, e ) => {
	      acc ++ unifierPaths(
		e
	      )( nameSpaceToString, varToString, tagToString ).map( extendPath( _, innerFunctor ) )
	    }
	  }
	)
      }
    }
  }

  def filteredPaths(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag],
    filter : CnxnCtxtLabel[Namespace,Var,Tag] => Boolean
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : List[String] = {
    def extendPath( path : String, innerFunctor : String ) = {
      path match {
	case "" => innerFunctor + ""
	case _ => innerFunctor + "." + path
      }
    }
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	List( tagToString( tag ) )
      }      
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	//List( varToString( v ) )
	List( "" )
      }
      case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
	( List[String]() /: innerFactuals.filter( filter ) )(
	  {
	    ( acc, e ) => {
	      acc ++ filteredPaths(
		e, filter
	      )( nameSpaceToString, varToString, tagToString ).map( extendPath( _, innerFunctor ) )
	    }
	  }
	)
      }
    }
  }

  def toMongoQuery(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	( tagToString( tag ) $exists true ) ++ ( tagSelector $exists true )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	( varToString( v ) $exists true ) ++ ( varSelector $exists true )
      }
      case ccb@CnxnCtxtBranch( functor, factuals ) => {	
	( MongoDBObject.empty /: paths( ccb ) )(
	  {
	    ( acc, e ) => {
	      acc ++ ( e $exists true )
	    }
	  }
	)

      }
    }
  }

  def toMongoUnificationQuery(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	( tagToString( tag ) $exists true ) ++ ( tagSelector $exists true )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	( varToString( v ) $exists true ) ++ ( varSelector $exists true )
      }
      case ccb@CnxnCtxtBranch( functor, factuals ) => {	
	( MongoDBObject.empty /: unifierPaths( ccb ) )(
	  {
	    ( acc, e ) => {
	      acc ++ ( e $exists true )
	    }
	  }
	)

      }
    }
  }

  def queryBindings(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag],
    src : DBObject
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : Seq[( String, DBObject )] = {
    for(
      path <- filteredPaths(
	cnxn,
	{
	  case CnxnCtxtLeaf( Left( tag ) ) => false
	  case CnxnCtxtLeaf( Right( tag ) ) => true
	  case CnxnCtxtBranch( _, _ ) => true
	}
      )(
	nameSpaceToString, varToString, tagToString
      );
      accessors = path.split( '.' )
    ) yield {
      ( accessors.last, ( src /: accessors )( ( acc, e ) => acc.get( e ).asInstanceOf[DBObject] ) )
    }
  }
}

trait CnxnMongoObject[Namespace,Var,Tag]
extends CnxnMongoSelectors {
  self : CnxnCtxtInjector[Namespace,Var,Tag]
         with CnxnString[Namespace,Var,Tag] =>
	 /* with Blobify with UUIDOps */   
 
  def fromJSON(
    json : JValue
  )(
    implicit toNameSpace : String => Namespace,
    toVar : String => Var,
    toTag : String => Tag
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    json match {      
      case JObject( ( "_id", JObject( _ ) ) :: ( ns, JObject( contents ) ) :: Nil ) => {
	fromJSONInner( ns, contents )( toNameSpace, toVar, toTag )
      }
      case JObject( ( ns, JObject( contents ) ) :: Nil ) => {
	fromJSONInner( ns, contents )( toNameSpace, toVar, toTag )
      }
      case JObject( ( ns, JInt( _ ) ) :: ( selector, JInt( _ ) ) :: Nil ) => {
	new CnxnCtxtLeaf[Namespace,Var,Tag](
	  if ( selector.equals( tagSelector ) ) {
	    Left[Tag,Var]( toTag( ns ) )
	  }
	  else {
	    Right[Tag,Var]( toVar( ns ) )
	  }	  
	)
      }
      case _ => {
	throw new Exception( "ill-formatted JSON: " + json )
      }
    }
  }

  def fromJSONInner(
    nameSpace : String,
    fields : List[(String, org.json4s.JsonAST.JValue)]
  )(
    toNameSpace : String => Namespace,
    toVar : String => Var,
    toTag : String => Tag
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    new CnxnCtxtBranch[Namespace,Var,Tag](
      toNameSpace( nameSpace ),
      fields.map( 
	{
	  ( f ) => {
	    f match {
	      case ( ns, JObject( ( leaf, JInt( _ ) ) :: ( selector, JInt( _ ) ) :: Nil ) ) => {
		if ( selector.equals( tagSelector ) ) {
		  new CnxnCtxtBranch[Namespace,Var,Tag](
		    toNameSpace( ns ),
		    List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual](
		      new CnxnCtxtLeaf[Namespace,Var,Tag](
			Left[Tag,Var]( toTag( leaf ) )
		      )
		    )
		  )
		}
		else {
		  new CnxnCtxtLeaf[Namespace,Var,Tag](
		    Right[Tag,Var]( toVar( leaf ) )
		  )
		}		
	      }
	      case ( ns, JObject( ctnts ) ) => {
		fromJSONInner( ns, ctnts )( toNameSpace, toVar, toTag )
	      }
	      case _ => {
		throw new Exception( "ill-formatted JSON: " + f )
	      }
	    }
	  }
	}
      )
    )
  }

  def toJSON(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : JValue = {
    // This should insure that we have consistent JSON encoding of
    // CCL's for roundtrip
    parse(
      toMongoObject( cnxn )( nameSpaceToString, varToString, tagToString ).toString
    )
  }  

  def toMongoObject(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	MongoDBObject( tagToString( tag ) -> 1, tagSelector -> 1 )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	MongoDBObject( varToString( v ) -> 1, varSelector -> 1 )
      }
      case CnxnCtxtBranch( functor, factuals ) => {
	val functorStr = nameSpaceToString( functor )
	val mdbo = 
	  toMongoObjectInner( cnxn )(
	    nameSpaceToString,
	    varToString,
	    tagToString
	  )

	MongoDBObject( functorStr -> mdbo )
      }
    }
  }

  def toMongoObjectInner(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  )(
    implicit nameSpaceToString : Namespace => String,
    varToString : Var => String,
    tagToString : Tag => String
  ) : DBObject = {
    cnxn match {
      case CnxnCtxtLeaf( Left( tag ) ) => {
	MongoDBObject( tagToString( tag ) -> 1, tagSelector -> 1 )
      }
      case CnxnCtxtLeaf( Right( v ) ) => {
	// BUGBUG : lgm -- incorrect semantics
	MongoDBObject( varToString( v ) -> 1, varSelector -> 1 )
      }
      case CnxnCtxtBranch( functor, factuals ) => {
	val functorStr = nameSpaceToString( functor )
	val factArray = new Array[Any]( factuals.length )
	
	val fields : List[( String, Any )] =
	  ( List[( String, Any )]() /: factuals )(
	    {
	      ( acc, fact ) => {
		fact match {
		  case CnxnCtxtBranch( innerFunctor, innerFactuals ) => {
		    val innerFunctorStr = nameSpaceToString( innerFunctor )
		    val imdbo =
		      toMongoObjectInner( fact )(
			nameSpaceToString,
			varToString,
			tagToString
		      )
		    acc ++ List( innerFunctorStr -> imdbo )
		  }
		  case CnxnCtxtLeaf( Left( tag ) ) => {
		    acc ++ List( tagToString( tag ) -> 1, tagSelector -> 1 )
		  }
		  case CnxnCtxtLeaf( Right( v ) ) => {
		    // BUGBUG : lgm -- incorrect semantics
		    val vStr = varToString( v )
		    val vRcrd = MongoDBObject( List( vStr -> 1, varSelector -> 1 ) )
		    acc ++ List( vStr -> vRcrd )
		  }
		}
	      }
	    }
	  )

	MongoDBObject( fields )
      }
    }
  }

  def fromMongoObject(
    src : DBObject
  )(
    implicit toNameSpace : String => Namespace,
    toVar : String => Var,
    toTag : String => Tag
  ) : CnxnCtxtLabel[Namespace,Var,Tag] = {
    fromJSON( parse( src.toString ) )( toNameSpace, toVar, toTag )
  }
}

trait JSONIfy[Namespace,Var,Tag] {  
  type CMQ = CnxnMongoQuery[Namespace,Var,Tag] with CnxnCtxtInjector[Namespace,Var,Tag] with CnxnString[Namespace,Var,Tag] with Blobify with UUIDOps
  type CMO = CnxnMongoObject[Namespace,Var,Tag] with CnxnCtxtInjector[Namespace,Var,Tag] with CnxnString[Namespace,Var,Tag] with Blobify with UUIDOps
  @transient
  var _CnxnMongoQuerifier : Option[CMQ] = None

  def CnxnMongoQuerifier() : CMQ = {    
    _CnxnMongoQuerifier match {
      case Some( cmq : CMQ ) => cmq
      case None | null => {
        val cmq : CMQ = 
          new CnxnMongoQuery[Namespace,Var,Tag] with CnxnCtxtInjector[Namespace,Var,Tag] with CnxnString[Namespace,Var,Tag] with Blobify with UUIDOps { }
        _CnxnMongoQuerifier = Some( cmq )
        cmq
      }
    }
    
  }
  @transient
  var _CnxnMongoObjectifier : Option[CMO] = None
  def CnxnMongoObjectifier() : CMO = {
    _CnxnMongoObjectifier match {
      case Some( cmo : CMO ) => cmo
      case None | null => {
        val cmo : CMO = new CnxnMongoObject[Namespace,Var,Tag] with CnxnCtxtInjector[Namespace,Var,Tag] with CnxnString[Namespace,Var,Tag] with Blobify with UUIDOps {}
        _CnxnMongoObjectifier = Some( cmo )
        cmo
      }
    }
  }

  case class CCLStringConversionsWrapper(
    s : String
  ) extends  CnxnCtxtInjector[Namespace,Var,Tag]
       with CnxnString[Namespace,Var,Tag] {
    def toCCL() : CnxnCtxtLabel[String,String,String] = {
      fromTermString( s ) match {
	case Some( ccl ) => ccl
	case None => throw new Exception( "failed to parse" )
      }
    }
  }

  object CCLStringConversions {
    implicit def toConverter( s : String ) : CCLStringConversionsWrapper =
      CCLStringConversionsWrapper( s )
    def apply( s : String ) : CCLStringConversionsWrapper =
      CCLStringConversionsWrapper( s )
  }

  case class CCLConversionsWrapper(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    override val nameSpaceToString : Namespace => String,
    override val varToString : Var => String,
    override val tagToString : Tag => String
  ) extends CnxnMongoQuery[Namespace,Var,Tag]
  with CnxnMongoObject[Namespace,Var,Tag]
  with CnxnStringDefaults[Namespace,Var,Tag]
  with CnxnCtxtInjector[Namespace,Var,Tag]
  with CnxnString[Namespace,Var,Tag] {
    def toMongoQuery() : DBObject =
      toMongoQuery( ccl )( nameSpaceToString, varToString, tagToString )
    def toJSON() : JValue =
      toJSON( ccl )( nameSpaceToString, varToString, tagToString )
    def toMongoObject() : DBObject = 
      toMongoObject( ccl )( nameSpaceToString, varToString, tagToString )
  }

  object CCLConversions
       extends CnxnStringDefaults[Namespace,Var,Tag] {
    implicit def toConverter(
      ccl : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : CCLConversionsWrapper =
      CCLConversionsWrapper(
	ccl, nameSpaceToString, varToString, tagToString
      )
    def apply(
      ccl : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : CCLConversionsWrapper =
      CCLConversionsWrapper(
	ccl, nameSpaceToString, varToString, tagToString
      )
  }
}

package usage {
  object MyCnxnMongoQuery extends JSONIfy[String,String,String] {    
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
  object MyCnxnMongoData {
    import MyCnxnMongoQuery._
    val ccl1 : CnxnCtxtLabel[String,String,String] =
      CCLStringConversions( "t1( a( 1 ), b( \"a string is born\" ), c( true ) )" ).toCCL()
    val ccl2 : CnxnCtxtLabel[String,String,String] =
      CCLStringConversions( "t1( a( 1 ), b( X ), c( true ) )" ).toCCL()
    val mdbo1 : DBObject = MyCCLConversions( ccl1 ).toMongoObject()
    val mdbo2 : DBObject = MyCCLConversions( ccl2 ).toMongoObject()
    val json1 = mdbo1.toString
    val json2 = mdbo2.toString
    val jv1 = parse( json1 )
    val jv2 = parse( json2 )
    val ccl1rt =
      CnxnMongoObjectifier.fromJSON( jv1 )( ( s : String ) => s, ( s : String ) => s, ( s : String ) => s )
    val ccl2rt =
      CnxnMongoObjectifier.fromJSON( jv2 )( ( s : String ) => s, ( s : String ) => s, ( s : String ) => s )
    val mqry1 : DBObject = MyCCLConversions( ccl1 ).toMongoQuery()
    val mqry2 : DBObject = MyCCLConversions( ccl2 ).toMongoQuery()
    val varPaths =
      CnxnMongoQuerifier.filteredPaths(
	ccl2,
	{
	  case CnxnCtxtLeaf( Left( tag ) ) => false
	  case CnxnCtxtLeaf( Right( tag ) ) => true
	  case CnxnCtxtBranch( _, _ ) => true
	}
      )(
	( s : String ) => s, ( s : String ) => s, ( s : String ) => s
      )
    val groundPaths =
      CnxnMongoQuerifier.filteredPaths(
	ccl2,
	{
	  case CnxnCtxtLeaf( Left( tag ) ) => true
	  case CnxnCtxtLeaf( Right( tag ) ) => false
	  case CnxnCtxtBranch( _, _ ) => true
	}
      )(
	( s : String ) => s, ( s : String ) => s, ( s : String ) => s
      )
    val qbs1 =
      CnxnMongoQuerifier.queryBindings( ccl2, mdbo1 )(
	( s : String ) => s, ( s : String ) => s, ( s : String ) => s
      )
  }
}
