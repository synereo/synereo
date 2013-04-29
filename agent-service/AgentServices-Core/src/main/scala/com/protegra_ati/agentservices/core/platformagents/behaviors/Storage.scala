package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents.BasePlatformAgent
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.{Being, PersistedKVDBNodeRequest, PersistedKVDBNodeResponse}
import com.protegra_ati.agentservices.store.util.Severity
import java.net.URI

trait Storage {
  self: BasePlatformAgent =>

  var _dbLocation: URI = null
  var _dbQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = null

  def initDb(config: ConfigurationManager) {
    _dbLocation = config.dbSelf
  }

  def loadStorageQueue() {
    _dbQ = createNode(_dbLocation, List())
  }

  def deleteDataForSelf(cnxn: AgentCnxnProxy, dataToDelete: Data) {
    dataToDelete match {
      case x: Connection => {
        processDeleteConnection(cnxn, x)
      }
      case _ => {
        processDeleteDataForSelf(cnxn, dataToDelete)
      }
    }
  }

  protected def processDeleteConnection(cnxn: AgentCnxnProxy, connection: Connection) {
    //delete connection in target connection list
    delete(_dbQ, cnxn, connection.toStoreKey)

    //drop the write cnxn collection
    drop(_dbQ, connection.writeCnxn)

    //don't drop the read cnxn collection - the other party in the connection will do that.
  }

  protected def processDeleteDataForSelf(cnxn: AgentCnxnProxy, dataToDelete: Data) {
    delete(_dbQ, cnxn, dataToDelete.toStoreKey)
    deleteDataForAllConnections(cnxn, dataToDelete)
  }

  protected def deleteDataForAllConnections(cnxn: AgentCnxnProxy, dataToDelete: Data) {
    val search = Connection.SEARCH_ALL
    fetch[Connection](_dbQ, cnxn, search.toSearchKey(), handleDeleteDataByConnectionFetch(_: AgentCnxnProxy, _: Connection, dataToDelete))
  }

  protected def handleDeleteDataByConnectionFetch(cnxn: AgentCnxnProxy, conn: Connection, dataToDelete: Data) {
    delete(_dbQ, conn.writeCnxn, dataToDelete.toStoreKey)
  }

  def updateDataById(cnxn: AgentCnxnProxy, newData: Data) {
    deleteDataById(cnxn, newData)
  }

  def deleteDataById[T <: Data](cnxn: AgentCnxnProxy, newData: T) {
    report("[Thread %s] deleteDataById called for cnxn %s  (storeKey: %s)"
      .format(Thread.currentThread.getName, cnxn, newData.toStoreKey), Severity.Debug)

    // Fetch data to delete, or if no data to delete, store newData
    // must call key before toDeleteKey to get the proper id
    val dataKey = newData.toStoreKey

    def handlerElse() {
      store(_dbQ, cnxn, dataKey, Serializer.serialize[Data](newData))
    }

    fetchListOrElse[Data](_dbQ, cnxn, newData.toDeleteKey(), handleDeleteAfterFetch(_: AgentCnxnProxy, _: List[Data], newData))(0, 0, () => handlerElse())
  }

  //could use a notion of retries if it isn't safe to delete
  protected def handleDeleteAfterFetch(cnxn: AgentCnxnProxy, dataToDelete: List[Data], newData: Data) {
    // Call store if data is empty; otherwise attempt to delete
    dataToDelete match {
      case Nil => store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[Data](newData))
      case _ => {
        // Delete all data
        var dataDeleted = false
        dataToDelete.map(x => {
          if (x != null && x != newData) {
            delete(_dbQ, cnxn, x.toStoreKey)
            dataDeleted = true
          }
        })

        if (dataDeleted) {
          store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[Data](newData))
        }
      }
    }
  }

  def updateDataBySearch[T <: Data](cnxn: AgentCnxnProxy, search: T, newData: Data) {
    //this will delete ALL occurrences of the specified data in the search object
    deleteDataBySearch(cnxn, search, newData)
  }

  def deleteDataBySearch[T <: Data](cnxn: AgentCnxnProxy, search: T, newData: Data) {
    def handlerElse() {
      store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[Data](newData))
    }

    fetchListOrElse[Data](_dbQ, cnxn, search.toSearchKey, handleDeleteAfterFetch(_: AgentCnxnProxy, _: List[Data], newData))(1, 0, () => handlerElse())
  }
}
