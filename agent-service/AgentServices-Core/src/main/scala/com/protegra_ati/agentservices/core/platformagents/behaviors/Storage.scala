package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import java.net.URI
import net.lag.configgy._
import com.protegra_ati.agentservices.core.schema.util._

trait Storage
{
  self: BasePlatformAgent =>

  var _dbLocation: URI = null
  var _dbQ: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] = null //persistedJunction
  var _dbConfigFileName: Option[String] = None

  def initDb(configUtil: Config, dbConfigFileName: Option[String])
  {
    val dbSelfMapKey = "db.self"
    _dbLocation = loadFirstURI(configUtil.getConfigMap(dbSelfMapKey))
    _dbConfigFileName = dbConfigFileName
  }

//  def initDb(dbLocation: URI, dbConfigFileName: Option[String])
//  {
//    _dbLocation = dbLocation
//    _dbConfigFileName = dbConfigFileName
//  }

  def loadStorageQueue() =
  {
    _dbQ = createNode(_dbLocation, List(), _dbConfigFileName)
  }

  def deleteDataForSelf(cnxn: AgentCnxnProxy, dataToDelete: Data) =
  {
    dataToDelete match {
      case x: Connection => {
        processDeleteConnection(cnxn, x)
      }
      case _ => {
        processDeleteDataForSelf(cnxn, dataToDelete)
      }
    }
  }

  protected def processDeleteConnection(cnxn: AgentCnxnProxy, connection: Connection) =
  {
    //delete connection in target connection list
    delete(_dbQ, cnxn, connection.toStoreKey)

    //drop the write cnxn collection
    drop(_dbQ, connection.writeCnxn)

    //don't drop the read cnxn collection - the other party in the connection will do that.
  }

  protected def processDeleteDataForSelf(cnxn: AgentCnxnProxy, dataToDelete: Data) =
  {
    delete(_dbQ, cnxn, dataToDelete.toStoreKey)

    deleteDataForAllConnections(cnxn, dataToDelete)
  }

  protected def deleteDataForAllConnections(cnxn: AgentCnxnProxy, dataToDelete: Data) =
  {
    val search = Connection.SEARCH_ALL
    fetch[ Connection ](_dbQ, cnxn, search.toSearchKey, handleDeleteDataByConnectionFetch(_: AgentCnxnProxy, _: Connection, dataToDelete))
  }

  protected def handleDeleteDataByConnectionFetch(cnxn: AgentCnxnProxy, conn: Connection, dataToDelete: Data) =
  {
    delete(_dbQ, conn.writeCnxn, dataToDelete.toStoreKey)
  }

  def updateDataById(cnxn: AgentCnxnProxy, newData: Data)
  {
    deleteDataById(cnxn, newData)
  }

  def deleteDataById [T<:Data](cnxn: AgentCnxnProxy, newData: T) : Unit =
  {
    // Fetch data to delete, or if no data to delete, store newData
    def handlerElse = store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[ Data ](newData))
    fetchListOrElse[ Data ](_dbQ, cnxn, newData.toDeleteKey, handleDeleteAfterFetch(_: AgentCnxnProxy, _: List[ Data ], newData))(1, 0, () => handlerElse)
  }

  //could use a notion of retries if it isn't safe to delete
  protected def handleDeleteAfterFetch(cnxn: AgentCnxnProxy, dataToDelete: List[ Data ], newData: Data)
  {
    // Call store if data is empty; otherwise attempt to delete
    dataToDelete match {
      case Nil => store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[ Data ](newData))
      case _ => dataToDelete.map(x => safeDelete(cnxn, x, newData))
    }
  }

  //exception to the convention of newData, oldData
  protected def safeDelete(cnxn: AgentCnxnProxy, dataToDelete: Data, dataToPreserve: Data) =
  {
    //this check prevents the race condition occurring where the new data is saved before the fetch is finished
    // We only want to store the data if we've deleted old data.  We do not want to
    // call store in case we're not deleting old data, otherwise we'll end up with dupes
    if ( dataToDelete != null && dataToDelete != dataToPreserve ) {
      delete(_dbQ, cnxn, dataToDelete.toStoreKey)
      store(_dbQ, cnxn, dataToPreserve.toStoreKey, Serializer.serialize[ Data ](dataToPreserve))
    }
  }

  def updateDataBySearch [T<:Data](cnxn: AgentCnxnProxy, search: T, newData: Data)
  {
    //this will delete ALL occurrences of the specified data in the search object
    deleteDataBySearch(cnxn, search, newData)
  }

  def deleteDataBySearch [T<:Data](cnxn: AgentCnxnProxy, search: T, newData: Data) : Unit =
  {
    def handlerElse = store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[ Data ](newData))
    fetchListOrElse[ Data ](_dbQ, cnxn, search.toSearchKey, handleDeleteAfterFetch(_: AgentCnxnProxy, _: List[ Data ], newData))(1, 0, () => handlerElse)
  }
}