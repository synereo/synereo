// -*- mode: Scala;-*- 
// Filename:    mongoQueryProbe.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug  6 14:24:40 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

object MongoQueryProbe {
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope.Being._
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor._
  import com.biosimilarity.lift.model.store._
  import com.mongodb.casbah.Imports._

  def erqlInstance() : CnxnCtxtLabel[String,String,String] = {
    DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()( Left[String,String]( java.util.UUID.randomUUID().toString ) ).getOrElse( throw new \
Exception( "error making evalRequestLabel" ) )
  }
  def kQuery(
       channel : StdEvalChannel,
       collectionName : String,
       ptn : CnxnCtxtLabel[String,String,String]
      ) : Option[DBObject] = {
    for(
      pm <- channel.cache.persistenceManifest;
      qry <- pm match {
	case mpm : MongoDBManifest => {
          mpm.kqueryDBObject( collectionName, ptn )
	}
        case _ => {
          None
	}
      }
    ) yield {
      qry
    }
  }
}
