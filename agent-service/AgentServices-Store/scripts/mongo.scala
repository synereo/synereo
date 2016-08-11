// -*- mode: Scala;-*- 
// Filename:    mongo.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 20 14:59:28 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.mongodb.casbah.Imports._
import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.model.store._
import scala.util.continuations._
import scala.concurrent.{Channel=>Chan, _}

object ExerciseMongo {
  import com.protegra_ati.agentservices.store.mongo.usage._
  import com.protegra_ati.agentservices.store.extensions.URIExtensions._
  import com.protegra_ati.agentservices.store.extensions.MonikerExtensions._
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  import AgentKVDBMongoScope._
  import Being._
  import AgentKVDBNodeFactory._
  import CnxnConversionStringScope._

  case class MyLabel( b : Boolean, i : Int, s : String, r : Option[MyLabel] )

  val auc1 = AgentUseCase( None )
  import auc1._

  val agentNode1 =
    agent[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( "/TheAgency" ) 

  val pd1 =
    agentNode1.cache.persistenceManifest.getOrElse(
      throw new Exception( "!" )
    ).asInstanceOf[MongoDBManifest]  

  val cnxnGlobal =
    new acT.AgentCnxn("Global".toURI, "", "Global".toURI)

  val mL1 =
    CnxnConversionStringScope.partialCaseClassDerivative(
      MyLabel( true, 0, "zero", None ),
      List( ( "b", "b" ) )
    )

  val rcrd1 =
    agentNode1.cache.asStoreRecord(
      mL1, "!"
    ).getOrElse( throw new Exception( "!" ) )  

  val mrcrd1 =
    agentNode1.cache.toMongoObject(
      rcrd1
    )(
      agentNode1.cache.nameSpaceToString,
      agentNode1.cache.varToString,
      agentNode1.cache.tagToString
    )  

  def doPut( v : String ) = {
    reset { agentNode1.put( cnxnGlobal )( mL1, v ) }
  }
  def doGet() = {
    reset {
      for( e <- agentNode1.get( cnxnGlobal )( mL1 ) ) { println( e ) }
    }
  }
  def doStore = {
    val ( pmgj1, pd1, Some( mongoCollName1 ) ) = agentNode1.getLocalPartitionActuals( cnxnGlobal )
    pmgj1.cache.store( mongoCollName1 )( rcrd1 )(
      agentNode1.cache.nameSpaceToString,
      agentNode1.cache.varToString,
      agentNode1.cache.tagToString
    )
  }
  def doDelete = {
    val ( pmgj1, pd1, Some( mongoCollName1 ) ) = agentNode1.getLocalPartitionActuals( cnxnGlobal )
    pmgj1.cache.delete( mongoCollName1, mL1 )(
      agentNode1.cache.nameSpaceToString,
      agentNode1.cache.varToString,
      agentNode1.cache.tagToString,
      agentNode1.cache.labelToNS.getOrElse( ( s : String ) => s )
    )
  }
}

object MongoDetails {
  import ExerciseMongo._ 
  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.{read, write}

  val ( pmgj1, pd1, Some( mongoCollName1 ) ) =
    agentNode1.getLocalPartitionActuals( cnxnGlobal )
  val clntSess1 =
    pmgj1.cache.acquireSession( pmgj1.cache.sessionURIFromConfiguration )
  val mc1 =
    clntSess1.getDB( pmgj1.cache.defaultDB )( "GlobalGlobal" )
  lazy val qry1 =
    pmgj1.cache.query( "GlobalGlobal", mL1 ).getOrElse( throw new Exception( "!" ) )
  lazy val qryRslts1 =
    pmgj1.cache.executeWithResults( "GlobalGlobal", qry1 )
  lazy val qryRslts10JSON =
    parse( qryRslts1( 0 ).toString )
}
