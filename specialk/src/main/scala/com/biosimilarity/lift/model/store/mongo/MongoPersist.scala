package com.biosimilarity.lift.model.store.mongo

import com.biosimilarity.lift.model.store.CnxnMongoObject
import com.biosimilarity.lift.model.store.CnxnMongoQuery
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.CnxnString
import com.biosimilarity.lift.model.store.Persist
import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.Blobify

import com.biosimilarity.lift.lib.UUIDOps
import com.biosimilarity.lift.lib.ConfigurationTrampoline
import com.biosimilarity.lift.lib.zipper._

import com.mongodb.casbah.Imports._

trait MongoStoreConfiguration
extends ConfigurationTrampoline {
  def URI                      : String =
    configurationFromFile.get( "URI" ).getOrElse( bail() )
  def dbHost                   : String =
    configurationFromFile.get( "dbHost" ).getOrElse( bail() )
  def dbPort                   : String =
    configurationFromFile.get( "dbPort" ).getOrElse( bail() )
  def dbUser                   : String =
    configurationFromFile.get( "dbUser" ).getOrElse( bail() )
  def dbPwd                    : String =
    configurationFromFile.get( "dbPwd" ).getOrElse( bail() )
  def driver                   : String =
    configurationFromFile.get( "driver" ).getOrElse( bail() )
  def dbRoot                   : String =
    configurationFromFile.get( "dbRoot" ).getOrElse( bail() )
  def createDB                 : Boolean =
    configurationFromFile.get( "createDB" ).getOrElse( bail() ).toBoolean
  def indent                   : Boolean =
    configurationFromFile.get( "indent" ).getOrElse( bail() ).toBoolean
  def resourceType             : String =
    configurationFromFile.get( "resourceType" ).getOrElse( bail() )
  def queryServiceType         : String =
    configurationFromFile.get( "queryServiceType" ).getOrElse( bail() )
  def queryServiceVersion      : String =
    configurationFromFile.get( "queryServiceVersion" ).getOrElse( bail() )
  def managementServiceType    : String =
    configurationFromFile.get( "managementServiceType" ).getOrElse( bail() )
  def managementServiceVersion : String =
    configurationFromFile.get( "managementServiceVersion" ).getOrElse( bail() )  

  def valueStorageType : String =
    configurationFromFile.get( "valueStorageType" ).getOrElse( bail() )  
  def continuationStorageType : String =
    configurationFromFile.get( "continuationStorageType" ).getOrElse( bail() )  
  def tmpDirStr : String =
    configurationFromFile.get( "tmpDirStr" ).getOrElse( bail() )
}

trait MongoResultsParser
extends CnxnMongoObject[String,String,String]
with CnxnCtxtInjector[String,String,String] 
with CnxnString[String,String,String]
with Blobify with UUIDOps {
}

trait BaseMongoPersist extends Persist[MongoClient,DBObject]
with MongoStoreConfiguration
with MongoResultsParser
{
  override def configFileName: Option[String] = None

  override def configurationDefaults: ConfigurationDefaults =
  {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  @transient
  private final val pool = MongoSessionPool

  def clientSessionFromConfig: MongoClient =
  {
    //new ClientSession(dbHost, dbPort.toInt, dbUser, dbPwd)
    // BUGBUG -- lgm : add in config parameters!!!
    MongoClient()
  }

  private def clientSessionFromPool: MongoClient =
  {
    pool.borrowClientSession(dbHost, dbPort.toInt, dbUser, dbPwd)
  }

  /**
   * Any method caller should make sure to return the connection to the pool!
   * @param collectionName
   * @return
   */
  def open(collectionName: String) : MongoClient

  // Note: leaveOpen is being ignored
  def checkIfDBExists(collectionName: String, leaveOpen: Boolean): Boolean

  def _checkIfDBExists(clientSession:MongoClient, collectionName: String): Boolean


  // Note: leaveOpen is being ignored
  def checkIfDBExistsAndCreateIfNot(collectionName: String, leaveOpen: Boolean): Boolean

  def _checkIfDBExistsAndCreateIfNot(clientSession: MongoClient, collectionName: String): Boolean


  def createDb(clientSession: MongoClient, collectionName: String) : Unit


  def insertUpdate( recordType : String )( collectionName : String, key : String, value : String ) : Unit



  def exists( recordType : String )(collectionName: String, key: String): Boolean

  def _exists( recordType : String, clientSession:MongoClient )(collectionName: String, key: String): Boolean


  def update( recordType : String )(collectionName: String, key: String, value: String) : Unit

  def _update( recordType : String, clientSession: MongoClient )(collectionName: String, key: String, value: String) : Unit


  def insert( recordType : String )( collectionName : String, key : String, value : String )

  def _insert( recordType : String, clientSession: MongoClient )( collectionName : String, key : String, value : String )


  def delete( recordType : String )(collectionName: String, key: String) : Unit


  def count(collectionName: String): Int


  def drop(collectionName: String) : Unit


  //executes defer exception handling to parent method
  def execute(collectionName: String, query: String): Unit

  //executes defer exception handling to parent method
  def _execute(clientSession: MongoClient, collectionName: String, query: String): Unit


  def execute(collectionName: String, queries: List[String]): Unit

  def _execute(clientSession: MongoClient, collectionName: String, queries: List[String]): Unit


  def executeScalar(collectionName: String, query: String): String

  def _executeScalar(clientSession: MongoClient, collectionName: String, query: String): String


  def executeWithResults( collectionName : String, queries : scala.List[String] ): scala.List[DBObject]

  def executeWithResults(  collectionName : String, query : String ) : scala.List[DBObject]


  def createTemplate( collectionName: String ) : String

  def replaceTemplate( recordType : String ) : String


  def existsTemplate( recordType : String ) : String

  def insertTemplate() : String

  def recordDeletionQueryTemplate( recordType : String ) : String
}
