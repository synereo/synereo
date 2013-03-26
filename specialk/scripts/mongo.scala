// -*- mode: Scala;-*- 
// Filename:    mongo.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 20 14:59:28 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package quicktest {

  import com.mongodb.casbah.Imports._
  import com.biosimilarity.lift.model.store._
  import scala.util.continuations._
  import scala.concurrent.{Channel=>Chan, _}

  object ExerciseMongo {
    import com.biosimilarity.lift.model.store.usage._
    import PersistedMonadicKVDBMongoNet._
    import Being._
    import PersistedMongoMolecularUseCase._
    import KinaseSpecifications._
    
    val node1 =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
	"localhost", 5672, "localhost", 5672
      ) match {
	case Left( n ) => n 
	case _ => throw new Exception( "!" )
      }
    val pd1 =
      node1.cache.persistenceManifest.getOrElse(
	throw new Exception( "!" )
      ).asInstanceOf[MongoDBManifest]
    
    val kinasePtnRAF = molPtnMap( RAFProto )
    val kamtRAF = cellCytoplasm.amt( kinasePtnRAF )
    val knsRAF1 = RAFProto.update( 1 )
    val RAFQry1 = mkMolQry( knsRAF1 )
    val rcrd1 =
      node1.cache.asStoreRecord(
	RAFQry1, 1.0
      ).getOrElse( throw new Exception( "!" ) )  
    val mrcrd1 =
      node1.cache.toMongoObject(
	rcrd1
      )(
	node1.cache.nameSpaceToString,
	node1.cache.varToString,
	node1.cache.tagToString
      )
    def doStore() = {
      node1.cache.store(
	pd1.storeUnitStr
      )( rcrd1 )(
	node1.cache.nameSpaceToString,
	node1.cache.varToString,
	node1.cache.tagToString
      )
    }
    def doGet() = {
      reset { for( e <- node1.get( RAFQry1 ) ) { println( e ) } }
    }
    def doSubscribe() = {
      reset { for( e <- node1.subscribe( RAFQry1 ) ) { println( e ) } }
    }
    def doPublish() = {
      reset { node1.publish( RAFQry1, 10.0 ) }
    }
    def doDoThatVoodoo() {
      println( "/* Storing */" )
      doStore()
      println( "/* Getting */" )
      doGet()
      println( "/* Subscribing */" )
      doSubscribe()
      println( "/* Publishing */" )
      doPublish()
      println( "/* Publishing */" )
      doPublish()
    }
  }
  
  object MongoDetails {
    import ExerciseMongo._
    import com.biosimilarity.lift.model.store.mongo._
    val spool1 =
      MongoSessionPool(
	node1.cache.sessionURIFromConfiguration
      )
    val clntSess1 = spool1.borrowObject()
    val mc1 =
      clntSess1.getDB( node1.cache.defaultDB )( pd1.storeUnitStr )
  }

}
