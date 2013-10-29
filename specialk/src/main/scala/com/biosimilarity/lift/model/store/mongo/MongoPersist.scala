package com.biosimilarity.lift.model.store.mongo

import com.biosimilarity.lift.model.store.CnxnMongoObject
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.CnxnString
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Persist
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.SessionURIConversionsT

import com.biosimilarity.lift.lib.UUIDOps
import com.biosimilarity.lift.lib.ConfigurationTrampoline

import com.mongodb.casbah.Imports._
import com.mongodb.util.JSON


import java.net.URI
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object MongoUtils extends SessionURIConversionsT {
  def getSessionURIFromTuple(
    host : String, port : Int, user : String, pwd : String
  ) : URI = {
    getSessionURIFromTuple( "mongodb" )( host, port, user, pwd, "" )
  }
}

object MongoConversions {
  import java.net.URI
  // import com.mongoDB.{ MongoClientURI => MCURI }
//   implicit def uriToMongoClientURI( uri : URI ) : MCURI = {   
//     new MCURI( uri.toString )
//   }
  implicit def uriToCasbahURI( uri : URI ) : MongoClientURI = {
    new MongoClientURI( uri.toString )
  }  
}

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
  def defaultDB                : String =
    configurationFromFile.get( "defaultDB" ).getOrElse( bail() )
  def defaultMongoOptions      : String =
    configurationFromFile.get( "defaultMongoOptions" ).getOrElse( bail() )
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
  
  def getSessionURIFromConfiguration : URI = {
    MongoUtils.getSessionURIFromTuple( dbHost, dbPort.toInt, dbUser, dbPwd )
  }  
}

trait MongoStoreConfigurationProxy
extends MongoStoreConfiguration {
  def underlyingConfiguration : MongoStoreConfiguration

  override def URI                      : String = underlyingConfiguration.URI
  override def dbHost                   : String = underlyingConfiguration.dbHost
  override def dbPort                   : String = underlyingConfiguration.dbPort
  override def dbUser                   : String = underlyingConfiguration.dbUser
  override def dbPwd                    : String = underlyingConfiguration.dbPwd
  override def driver                   : String = underlyingConfiguration.driver
  override def dbRoot                   : String = underlyingConfiguration.dbRoot
  override def createDB                 : Boolean = underlyingConfiguration.createDB
  override def indent                   : Boolean = underlyingConfiguration.indent
  override def resourceType             : String = underlyingConfiguration.resourceType
  override def queryServiceType         : String = underlyingConfiguration.queryServiceType
  override def queryServiceVersion      : String = underlyingConfiguration.queryServiceVersion
  override def managementServiceType    : String = underlyingConfiguration.managementServiceType
  override def managementServiceVersion : String = underlyingConfiguration.managementServiceVersion

  override def valueStorageType         : String = underlyingConfiguration.valueStorageType  
  override def continuationStorageType  : String = underlyingConfiguration.continuationStorageType  
  override def tmpDirStr                : String = underlyingConfiguration.tmpDirStr

  override def getSessionURIFromConfiguration : URI =
    underlyingConfiguration.getSessionURIFromConfiguration
  override def configFileName  : Option[String] =
    underlyingConfiguration.configFileName
  override def configurationDefaults: ConfigurationDefaults =
    underlyingConfiguration.configurationDefaults
}

trait StdMongoStoreConfiguration extends MongoStoreConfigurationProxy {
  // @transient
  // override final val underlyingConfiguration = MongoConfigInfo
  def underlyingConfiguration = MongoConfigInfoFactory.createMongoConfigInfo()
}

trait MongoResultsParser[Namespace,Var,Tag]
extends CnxnMongoObject[Namespace,Var,Tag]
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnString[Namespace,Var,Tag]
with Blobify with UUIDOps {
  implicit def fromString( s : String ) : CnxnCtxtLabel[String,String,String] = {
    fromTermString( s ) match {
      case Some( ccl ) => ccl.asInstanceOf[CnxnCtxtLabel[String,String,String]];
      case None => throw new Exception( "failed to parse" )
    }
  }
}

trait MongoStringResultsParser extends MongoResultsParser[String,String,String]

object MongoClientPool {
  import java.util.concurrent.ConcurrentHashMap

  private val clientMap = new ConcurrentHashMap[String, MongoClient]()

  private val MAX_CLIENT_CONNECTIONS = 150

  /**
   * Returns a MongoClient instance tied to the specified URI, creating a new
   * instance if necessary
   * @param uri
   * @return
   */
  def client(uri: URI):MongoClient = {
    val key = uri.toString
    if (clientMap.containsKey(key)) {
      clientMap.get(key)
    } else {
      val newClient = MongoClient(uri.getAuthority, MongoClientOptions(true, MAX_CLIENT_CONNECTIONS))
      val tmpClient = clientMap.putIfAbsent(key, newClient)
      if (tmpClient == null) {
        newClient // New client used
      } else {
        tmpClient // Client already present in mapping, return old client
      }
    }
  }
}

trait BaseMongoPersist[Namespace,Var,Tag]
extends Persist[MongoClient,DBObject]
with MongoResultsParser[Namespace,Var,Tag]
with StdMongoStoreConfiguration
{ 
  @transient
  val CnxnMongoStrObjectifier =
    new CnxnMongoObject[String,String,String]
    with CnxnCtxtInjector[String,String,String]
         with CnxnString[String,String,String]
	 with Blobify with UUIDOps {}  

  @transient
  lazy val sessionURIFromConfiguration : URI = getSessionURIFromConfiguration

  @transient
  lazy val client = MongoClientPool.client(sessionURIFromConfiguration)

  def wrapAction[S,T]( action : ( MongoClient, S ) => T ) : S => T = {
    ( s : S ) => {
      try {
        val ans : T = action( client, s )
        ans
      } 
      catch {
        case mxe : Exception => {
          mxe.printStackTrace
          throw mxe
        }
      }
    }
  }

  /**
   * Any method caller should make sure to return the connection to the pool!
   * @param collectionName
   * @return
   */
  
  def open( collectionName : String ) : MongoClient = {
    throw new NotImplementedException()
  }

  def openSafely( collectionName : String ) : Boolean = {
    throw new NotImplementedException()
  }

  // Note: leaveOpen is being ignored
  def checkIfDBExists( collectionName : String, leaveOpen : Boolean ) : Boolean = {
    // Note: Mongo queries will work whether a DB exists or not, so we skip the check
    true
  }

  def _checkIfDBExists(
    clientSession : MongoClient, collectionName : String
  ): Boolean = {
    // Note: Mongo queries will work whether a DB exists or not, so we skip the check
    true
  }

  // Note: leaveOpen is being ignored
  def checkIfDBExistsAndCreateIfNot(
    collectionName : String, leaveOpen : Boolean
  ) : Boolean = {
    // Note: Mongo queries will work whether a DB exists or not, so we skip the check/creation
    true
  }

  def _checkIfDBExistsAndCreateIfNot(
    clientSession : MongoClient, collectionName : String
  ): Boolean = {
    // Note: Mongo queries will work whether a DB exists or not, so we skip the check
    true
  }

  /* ---------------------------------------------------------------------
   *
   * The structure of the mongodb record is
   *
   * { "record" : { "key" : <mongoEncodingOfPrologTerm>, "value" : <blob> } }
   * 
   * --------------------------------------------------------------------- */

  def asRecord( recordType : String )(
    key : String, value : String
  ) : DBObject = {
    CnxnMongoStrObjectifier.toMongoObject(
      new CnxnCtxtBranch[String,String,String](
	recordType,
	List[CnxnCtxtLabel[String,String,String] with Factual](
	  new CnxnCtxtBranch[String,String,String](
	    "key",
	    List[CnxnCtxtLabel[String,String,String] with Factual](
	      new CnxnCtxtLeaf[String,String,String]( Left[String,String]( key ) )
	    )
	  ),
	  new CnxnCtxtBranch[String,String,String](
	    "value",
	    List[CnxnCtxtLabel[String,String,String] with Factual](
	      new CnxnCtxtLeaf[String,String,String]( Left[String,String]( value ) )
	    )
	  )
	)
      )
    )
  }

  def createDb(
    clientSession : MongoClient, collectionName : String
  ) : Unit = {
    // Note: Mongo queries will work whether a DB exists or not, so we skip the creation
  }

  def insertUpdate( recordType : String )(
    collectionName : String, key : String, value : String
  ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, recordType : String ) => {
        _upsert( recordType, clientSession )( collectionName, key, value )
      }
    )( recordType )
  }

  def insertUpdateRecord(
    record : DBObject, collectionName : String
  ) : Unit = {
    wrapAction[DBObject,Unit](
      ( clientSession : MongoClient, record : DBObject ) => {
        _upsertRecord( record, clientSession )( collectionName )
      }
    )( record )
  }

  def _upsertRecord( record : DBObject, clientSession : MongoClient )(
    collectionName : String
    ) : Unit = {
    com.biosimilarity.lift.lib.BasicLogService.tweet(
      (
        "**********************************************************" 
        + "\nmethod: " + "_upsertRecord"
        + "\nrecord: " + record
        + "\nclientSession: " + clientSession
        + "\n**********************************************************"
      )
    )
    // BUGBUG: updateRecord will always insert a new record; never update an existing one
    // To update a record using upsert in MongoDB, we need to satisfy 1 of 2 conditions:
    // 1. Set record ObjectId (_id) if using += (which we NEVER do)
    // 2. Supply a query with which MongoDB will update a record (which we do for _upsert),
    // and use the update() method with upsert=true
    // We should supply a query here as well instead of using +=
    val mc = clientSession.getDB( defaultDB )( collectionName )
    //mc += record
    val rcrdObj = record.get( "record" )
    if ( rcrdObj != null ) {
      val rcrdKey = rcrdObj.asInstanceOf[DBObject].get( "key" )
      com.biosimilarity.lift.lib.BasicLogService.tweet(
        (
          "**********************************************************" 
          + "\nmethod: " + "_upsertRecord"
          + "\nrecord: " + record
          + "\nclientSession: " + clientSession
          + "\n----------------------------------------------------------"
          + "\nrcrdKey: " + rcrdKey
          + "\n**********************************************************"
        )
      )
      mc.update(
        MongoDBObject( "record.key" -> rcrdKey ),
        record,
        true
      )
    }
    else {
      val kRcrdObj = record.get( "kRecord" )
      if ( kRcrdObj == null ) {
        throw new Exception( "invalid record shape: " + record )
      }
      else {
        val kRcrdKey = kRcrdObj.asInstanceOf[DBObject].get( "key" )
        com.biosimilarity.lift.lib.BasicLogService.tweet(
          (
            "**********************************************************" 
            + "\nmethod: " + "_upsertRecord"
            + "\nrecord: " + record
            + "\nclientSession: " + clientSession
            + "\n----------------------------------------------------------"
            + "\nkRcrdKey: " + kRcrdKey
            + "\n**********************************************************"
          )
        )
        
        mc.update(
          MongoDBObject( "kRecord.key" -> kRcrdKey ),
          record,
          true
        )
      }
    }    
  }

  def exists( recordType : String )(
    collectionName : String, key : String
  ): Boolean = {
    wrapAction(
      ( clientSession : MongoClient, recordType : String ) => {
	      _exists( recordType, clientSession )( collectionName, key )
      }
    )( recordType )
  }

  def _exists(
    recordType : String, clientSession : MongoClient
  )(
    collectionName : String, key : String
  ) : Boolean = {
    val existsQry = ( "record.key." + key ) $exists true
    clientSession.getDB(defaultDB)(collectionName).findOne(existsQry).size > 0
  }

  def update( recordType : String )(
    collectionName: String, key : String, value : String
  ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, recordType : String ) => {
        _upsert( recordType, clientSession )( collectionName, key, value )
      }
    )( recordType )
  }

  def insert( recordType : String )(
    collectionName : String, key : String, value : String
  ) = {
    wrapAction(
      ( clientSession : MongoClient, recordType : String ) => {
        _upsert( recordType, clientSession )( collectionName, key, value )
      }
    )( recordType )
  }

  def _upsert( recordType : String, clientSession: MongoClient )(
    collectionName : String, key : String, value : String
  ) = {
    val keyQry = ( "record.key." + key ) $exists true
    val mc = clientSession.getDB( defaultDB )( collectionName )
    mc.update(keyQry, asRecord( recordType )( key, value ), true)
  }


  def delete( recordType : String )(
    collectionName : String, key : String
  ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, recordType : String ) => {
	      _delete( recordType, clientSession )( collectionName, key )
      }
    )( recordType )
  }

  def deleteRecord( record : DBObject )(
    collectionName : String
  ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, record : DBObject ) => {
	      _deleteRecord( record, clientSession )( collectionName )
      }
    )( record )
  }

  def deleteRecordsMatchingKey( deleteKey : DBObject )(
    collectionName : String
  ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, deleteKey : DBObject ) => {
	      _deleteRecordsMatchingKey( deleteKey, clientSession )( collectionName )
      }
    )( deleteKey )
  }

  def _delete( recordType : String, clientSession : MongoClient )(
    collectionName : String, key : String
  ) : Unit = {
    val deleteKey = ( "record.key." + key ) $exists true
    _deleteRecordsMatchingKey( deleteKey, clientSession )( collectionName )
  }

  def _deleteRecord( record : DBObject, clientSession : MongoClient )(
    collectionName : String
  ) : Unit = {
    clientSession.getDB( defaultDB )( collectionName ).remove( record )
  }

  def _deleteRecordsMatchingKey( deleteKey : DBObject, clientSession : MongoClient )(
    collectionName : String
  ) : Unit = {
    clientSession.getDB( defaultDB )( collectionName ).remove( deleteKey )
  }

  def count( collectionName : String ) : Int = {
    throw new NotImplementedException
  }


  def drop( collectionName : String ) : Unit = {
    wrapAction( _drop )( collectionName )
  }

  def _drop( clientSession : MongoClient, collectionName : String ) : Unit = {
    clientSession.getDB( defaultDB )( collectionName ).drop()
  }

  //executes defer exception handling to parent method
  def execute( collectionName : String, query : String ) : Unit = {
    wrapAction(
      ( clientSession : MongoClient, collectionName : String ) => {
	      _execute( clientSession, collectionName, query )
      }
    )( collectionName )
  }

  //executes defer exception handling to parent method
  def _execute(
    clientSession : MongoClient, collectionName : String, query : String
  ) : Unit = {
    val qry = JSON.parse( query ).asInstanceOf[DBObject]
    val mc = clientSession.getDB( defaultDB )( collectionName )
    for( entry <- mc.find( qry ) ) {}
  }


  def execute( collectionName : String, queries : List[String] ): Unit = {
    wrapAction(
      ( clientSession : MongoClient, collectionName : String ) => {
	      _execute( clientSession, collectionName, queries )
      }
    )( collectionName )
  }

  def _execute(
    clientSession : MongoClient, collectionName : String, queries : List[String]
  ) : Unit = {    
    val mc = clientSession.getDB( defaultDB )( collectionName )
    for(
      qryStr <- queries;
      entry <- mc.find( JSON.parse( qryStr ).asInstanceOf[DBObject] )
    ) {}
  }

  def executeScalar( collectionName : String, query : String ): String = {
    wrapAction(
      ( clientSession : MongoClient, collectionName : String ) => {
	      _executeScalar( clientSession, collectionName, query )
      }
    )( collectionName )
  }

  def _executeScalar(
    clientSession : MongoClient, collectionName : String, query : String
  ) : String = {
    val qry = JSON.parse( query ).asInstanceOf[DBObject]
    val mc = clientSession.getDB( defaultDB )( collectionName )
    ( "" /: mc.find( qry ).toList )( _ + _ )
  }


  def executeWithResults(
    collectionName : String, queries : scala.List[String]
  ) : scala.List[DBObject] = {
    wrapAction(
      ( clientSession : MongoClient, collectionName : String ) => {
        _executeWithResults( clientSession, collectionName, queries )
      }
    )( collectionName )
  }

  def _executeWithResults(
    clientSession : MongoClient, collectionName : String, queries : scala.List[String]
  ) : scala.List[DBObject] = {
    val mc = clientSession.getDB( defaultDB )( collectionName )
    queries.flatMap(
      {
        ( qryStr : String ) => {
          val qry = JSON.parse( qryStr ).asInstanceOf[DBObject]
          mc.find( qry ).toList
        }
      }
    )
  }

  def executeWithResults(
    collectionName : String, query : String
  ) : scala.List[DBObject] = {
    wrapAction(
      ( clientSession : MongoClient, collectionName : String ) => {
	      _executeWithResults( clientSession, collectionName, query )
      }
    )( collectionName )
  }

  def _executeWithResults(
    clientSession : MongoClient, collectionName : String, query : String
  ) : scala.List[DBObject] = {
    val qry = JSON.parse( query ).asInstanceOf[DBObject]
    val mc = clientSession.getDB( defaultDB )( collectionName )
    mc.find( qry ).toList
  }


  def createTemplate( collectionName: String ) : String = {
    throw new NotImplementedException
  }

  def replaceTemplate( recordType : String ) : String = {
    throw new NotImplementedException
  }

  def existsTemplate( recordType : String ) : String = {
    throw new NotImplementedException
  }

  def insertTemplate() : String = {
    throw new NotImplementedException
  }

  def recordDeletionQueryTemplate( recordType : String ) : String = {
    throw new NotImplementedException
  }
}

package usage {
  object DataSet {
    val numberStr = "number( 7329 )"
    val streetStr = "street( \"39th Ave\" )"
    val cityStr = "city( \"Seattle\" )"
    val stateStr = "state( \"WA\" )"    
    val countryStr = "country( \"US\")"
    val addressStr = 
      (
	"address( "
	+ numberStr + " , "
	+ streetStr + " , "
	+ cityStr + " , "
	+ stateStr + " , "
	+ countryStr
	+ " )"
      );
    val profileStr =
      (
	"profile( "
	+ addressStr
	+ " )"
      )
    val userProfileStr =
      (
	"user( "
	+ profileStr
	+ " )"
      );
    val dataBlob = (
      "object Pi {" + "\n" +
      "	class PiIterator extends Iterable[BigInt]{" + "\n" +
      "	  var r:BigInt=0" + "\n" +
      "	  var q, t, k:BigInt=1" + "\n" +
      "	  var n, l:BigInt=3" + "\n" +
      "	  var nr, nn:BigInt=0" + "\n" +
      "                      " + "\n" +
      "	def iterator: Iterator[BigInt]=new Iterator[BigInt]{" + "\n" +
      "	  def hasNext=true" + "\n"+
      "	  def next():BigInt={" + "\n"+
      "     while((4*q+r-t) >= (n*t)) {" + "\n"+
      "       nr = (2*q+r)*l" + "\n"+
      "	      nn = (q*(7*k)+2+(r*l))/(t*l)" + "\n"+
      "	      q = q * k" + "\n"+
      "	      t = t * l" + "\n"+
      "	      l = l + 2" + "\n"+
      "	      k = k + 1" + "\n"+
      "	      n  = nn" + "\n"+
      "	      r  = nr" + "\n"+
      "    }" + "\n"+
      "	  val ret=n" + "\n"+
      "	  nr = 10*(r-n*t)" + "\n"+
      "	  n  = ((10*(3*q+r))/t)-(10*n)" + "\n"+
      "	  q  = q * 10" + "\n"+
      "	  r  = nr" + "\n"+
      "	  ret" + "\n"+
      "	  }" + "\n"+
      "	}" + "\n"+
      "}"
    )
  }
  object SimpleMongoPersist extends BaseMongoPersist[String,String,String] {    
    import DataSet._
    val userProfileCCL : CnxnCtxtLabel[String,String,String] = userProfileStr
  }
}
