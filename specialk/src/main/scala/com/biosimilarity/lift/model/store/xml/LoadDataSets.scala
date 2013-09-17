// -*- mode: Scala;-*- 
// Filename:    LoadDataSets.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 30 13:54:42 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml.datasets

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
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
import com.biosimilarity.lift.model.store.CnxnXML
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.CnxnXQuery
import com.biosimilarity.lift.lib._

import org.basex.api.xmldb.BXCollection
import org.basex.server.ClientSession
import org.basex.core.BaseXException
import org.basex.core.Context
import org.basex.core.cmd.{ List => BXList, _ }

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

import scala.xml._

import javax.xml.transform.OutputKeys
import java.util.UUID
import java.io.File

object CXQ extends CnxnXQuery[String,String,String]
 with CnxnCtxtInjector[String,String,String]
 with UUIDOps
 with Blobify
 with CnxnXML[String,String,String] {
   implicit def toPattern( s : String ) =
     fromCaseClassInstanceString( s )
   .getOrElse( null )
   .asInstanceOf[CnxnCtxtLabel[String,String,String]]
 }

trait BXGraphQueryResources {
  val datasetsDir = "src/main/resources/datasets/"
  val dbNames : List[String] =
    List( "GraphOne", "GraphTwo", "GraphThree", "GraphFour" )
  val dbSets : List[String] =
    List( "Route", "Connection", "Scale", "GraphGraphKV" )  

  val vertexQuery1 = "//VertexString"
  val vertexQuery2 = "//Pointed"
  val edgeQuery1 = "//Connection"
  val edgeQuery2 = "//EdgeName"
  val vertexRoleQueryTemplate =
    (
      "for $lbl in //LRBoundVertex"
      + " " 
      + "where $lbl//VertexString/String[@value=\"%V\"]"
      + " "
      + "return $lbl/VertexExprLabel/VertexVariable/LIdent/@value"
    );
  val vertexRoleQueryClientSessionTemplate =
    (
      "for $lbl in collection( '%COLLNAME%' )//LRBoundVertex"
      + " " 
      + "where $lbl//VertexString/String[@value=\"%V%\"]"
      + " "
      + "return $lbl/VertexExprLabel/VertexVariable/LIdent/@value"
    )

  val outerGraphExpr =
    "Connected( EdgeName( EdgeString( WS ) ), X, Y )"
  val firstVandG =
    "VertexSelection( LRBoundVertex( VELabel, V ), G )"
  val innerGraphExpr = 
    "VertexSelection( LRB, Connected( EdgeName( EdgeString( WS2 ) ), X1, Y1 ) )"
}

object BXUtilities extends BXGraphQueryResources

trait BaseXXMLUtilities
extends BaseXXMLStore
with BXGraphQueryResources
 with Blobify
 with ConfigurationTrampoline
 with UUIDOps {
   val outerGraphExprCCL =
    CXQ.fromCaseClassInstanceString( outerGraphExpr ).getOrElse( null ).asInstanceOf[CnxnCtxtLabel[String,String,String]]
  val outerGraphExprXQuery =
    CXQ.xqQuery( outerGraphExprCCL )
  val firstVandGCCL =
    CXQ.fromCaseClassInstanceString( firstVandG ).getOrElse( null ).asInstanceOf[CnxnCtxtLabel[String,String,String]]
  val firstVandGXQuery =
    CXQ.xqQuery( firstVandGCCL )
  val innerGraphExprCCL =
    CXQ.fromCaseClassInstanceString( innerGraphExpr ).getOrElse( null ).asInstanceOf[CnxnCtxtLabel[String,String,String]]
  val innerGraphExprXQuery =
    CXQ.xqQuery( innerGraphExprCCL )
 }

//object BXToBeDeprecated extends BaseXXMLUtilities
//{
//  override def configurationDefaults : ConfigurationDefaults = {
//    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
//  }
//
//  def populateDB(
//    dbNameStr : String,      // name of the database
//    contentFileStr : String  // root of the file name of the data
//  ) = {
//    // new database
//    val bxcoll = new BXCollection( dbNameStr, false )
//
//    // new document
//    val document =
//      bxcoll.createResource(
//	null,
//	XMLResource.RESOURCE_TYPE
//      ).asInstanceOf[XMLResource]
//
//    // add the data
//    document.setContent(
//      new java.io.File(
//	datasetsDir + contentFileStr + ".xml"
//      )
//    )
//
//    // store the data
//    bxcoll.storeResource( document )
//
//    // close the database
//    bxcoll.close
//
//    // open the database for operations
//    new BXCollection( dbNameStr, true )
//  }
//
//  def loadDataSets = {
//    dbNames.zip( dbSets ).map(
//      { ( dbNData ) => populateDB( dbNData._1, dbNData._2 ) }
//    )
//  }
//
//  lazy val dataSets = loadDataSets
//
//  def reportGraphs = {
//    for( dataSet <- dataSets.take( 3 ) ) {
//      val xqSrvc =
//	dataSet.getService(
//	  "XPathQueryService",
//	  "1.0"
//	).asInstanceOf[XPathQueryService]
//      val dbName = dataSet.getName
//      val vertexResourceSet = xqSrvc.query( vertexQuery2 )
//      println(
//	"// *************************************************************"
//      )
//      println(
//	"// Report for database: " + dbName
//      )
//      println(
//	"// *************************************************************"
//      )
//      println(
//	"Number of vertices in this graph is " + vertexResourceSet.getSize + "\n"
//      )
//      val vrsIter = vertexResourceSet.getIterator
//      while( vrsIter.hasMoreResources ) {
//	val vertexElemStr = vrsIter.nextResource.getContent.toString
//	val vertexElem = XML.loadString( vertexElemStr )
//	val vName = vertexElem \ "VertexString" \ "String" \ "@value"
//	val vertexRoleResourceSet =
//	  xqSrvc.query(
//	    vertexRoleQueryTemplate.replace( "%V", vName.toString )
//	  )
//	val vrrsIter = vertexRoleResourceSet.getIterator
//
//	println( "Vertex name is " + vName )
//	println( vertexElem \ "VertexString" )
//	while( vrrsIter.hasMoreResources ) {
//	  println(
//	    (
//	      "and plays in the role "
//	      +	vrrsIter.nextResource.getContent.toString
//	    )
//	  )
//	}
//	println( "" )
//      }
//
//      val edgeResourceSet = xqSrvc.query( edgeQuery2 )
//
//      println(
//	"\n\nNumber of edges in this graph is " + edgeResourceSet.getSize + "\n"
//      )
//
//      val ersIter = edgeResourceSet.getIterator
//      while( ersIter.hasMoreResources ) {
//	val edgeElemStr = ersIter.nextResource.getContent.toString
//	val edgeElem = XML.loadString( edgeElemStr )
//	val eName = edgeElem \ "EdgeString" \ "String" \ "@value"
//	println( "Edge name is " + eName )
//	println( edgeElem \ "EdgeString" )
//	println( "" )
//      }
//
//      println(
//	"// *************************************************************\n\n"
//      )
//    }
//  }
//}

object BX extends BaseXXMLUtilities
{  
  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  def populateDBClientSession(
    dbNameStr : String,      // name of the database
    contentFileStr : String  // root of the file name of the data
  ) = {    
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    // new database
    val clientSession = clientSessionFromConfig
    try {
      clientSession.setOutputStream( srvrRspStrm )
      // leftover from a test run
      clientSession.execute( new Open( dbNameStr ) )
      // so... get rid of it
      clientSession.execute( new DropDB( dbNameStr ) )

      val f1 =
	new java.io.File( datasetsDir + contentFileStr + ".xml" )
      if ( f1.exists ) {
	// fresh green field
	clientSession.execute( new CreateDB( dbNameStr ) )
	// add the content
	clientSession.execute(
	  new Add(
	    f1.getCanonicalPath,
	    dbNameStr	      
	  )
	)
      }
      else {
	throw new Exception( "test data file not found" )
      }
    }
    catch {
      case e : BaseXException => {
	val f1 =
	  new java.io.File( datasetsDir + contentFileStr + ".xml" )
	if ( f1.exists ) {
	  // fresh green field
	  clientSession.execute( new CreateDB( dbNameStr ) )
	  // add the content
	  clientSession.execute(
	    new Add(
	      f1.getCanonicalPath,
	      dbNameStr	      
	    )
	  )
	}
	else {
	  throw new Exception( "test data file not found" )
	}
      }
    }
    finally {
      clientSession.execute( new Close() )
    }
  }

  def loadDataSetsClientSession = {
    dbNames.zip( dbSets ).map(
      { ( dbNData ) => populateDBClientSession( dbNData._1, dbNData._2 ) }
    )
  }

  def loadDataSetsClientSession( tag : String ) = {
    dbNames.zip( dbSets ).map(
      { ( dbNData ) => populateDBClientSession( dbNData._1 + tag, dbNData._2 ) }
    )
  }

  def reportGraphsClientSession = {
    try {
      for( dbName <- dbNames.take( 3 ) ) {      
	val vertices =
	  executeWithResults(dbName,
	    (
	      "for $p in collection( '%COLLNAME%' )".replace(
		"%COLLNAME%",
		dbName
	      )
	      + vertexQuery2
	      + " return $p"
	    )
	  )

	println( 
	  "// *************************************************************"
	)
	println( 
	  "// Report for database: " + dbName 
	)      
	println( 
	  "// *************************************************************"
	)   
	
	println(
	  "Number of vertices in this graph is " + vertices.length + "\n"
	)
	
	for( vertexElem <- vertices ) {
	  val vName = vertexElem \ "VertexString" \ "String" \ "@value"
	  val vRoleQry =
	    vertexRoleQueryClientSessionTemplate.replace(
	      "%V%",
	      vName.toString
	    ).replace(
	      "%COLLNAME%",
	      dbName
	    )
	  
	  val vRoles = executeWithResults(dbName, vRoleQry )
	    
	  for( vRole <- vRoles ) {
	    println(
	      (
		"and plays in the role "
		+	vRole.toString
	      )
	    )
	  }
	  
	  println( "" )
	}           

	val edges = 
	  executeWithResults(dbName,
	    (
	      "for $p in collection( '%COLLNAME%' )".replace(
		"%COLLNAME%",
		dbName
	      )
	      + edgeQuery2
	      + " return $p"
	    )
	  )

	println(
	  "\n\nNumber of edges in this graph is " + edges.length + "\n"
	)
	
	for( edgeElem <- edges ) {
	  val eName = edgeElem \ "EdgeString" \ "String" \ "@value"
	  println( "Edge name is " + eName )	
	  println( edgeElem \ "EdgeString" )
	  println( "" )
	}      
	
	println( 
	  "// *************************************************************\n\n"
	)
      }
    }
    catch {
      case e : BaseXException => {
	BasicLogService.tweetTrace( e )
      }
    }
  }
}


