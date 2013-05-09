// -*- mode: Scala;-*- 
// Filename:    diesel.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  9 15:42:46 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.lift.model.store._
import scala.util.continuations._ 

object UseCase {
  import DSLCommLinkCtor._
  val ( client1, server1 ) = stdBiLink()
  val erql : CnxnCtxtLabel[String,String,String] =
    DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()( "SessionID" ).getOrElse( 
      throw new Exception( "error making evalRequestLabel" )
    )
  def doDrop() = {
    import com.biosimilarity.lift.model.store.mongo._
    val clntSess1 =
      MongoClientPool.client( client1.cache.sessionURIFromConfiguration )
    val mcExecLocal =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocolLocal" )
    val mcExecRemote =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocolRemote" )
    val mcExec =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocol" )

    mcExecLocal.drop
    mcExecRemote.drop
    mcExec.drop
  }
  def doPut() = {
    reset { client1.put( erql, DSLCommLink.mTT.Ground( ConcreteHL.Bottom ) ) }
  }
  def doGet() = {
    reset { for( e <- client1.subscribe( erql ) ) { println( e ) } }
  }
}
