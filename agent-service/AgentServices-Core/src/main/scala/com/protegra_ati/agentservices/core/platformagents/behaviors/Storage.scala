package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.biosimilarity.lift.lib.moniker._
import net.lag.configgy._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra.agentservicesstore.util._

trait Storage
{
  self: BasePlatformAgent =>

  var _dbLocation: URM = null
  var _dbQ: PartitionedStringMGJ = null //persistedJunction

  def initDb(configUtil: Config)
  {
    val dbSelfMapKey = "db.self"
    _dbLocation = loadFirstURM(configUtil.getConfigMap(dbSelfMapKey))
  }

  def initDb(dbLocation: URM)
  {
    _dbLocation = dbLocation
  }

  def loadStorageQueue() =
  {
    _dbQ = new PartitionedStringMGJ(_dbLocation, List(), None)
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
    val search = ConnectionFactory.createEmptyImmutableConnectionForSearch()
    fetch[ Connection ](_dbQ, cnxn, search.toSearchKey, handleDeleteDataByConnectionFetch(_: AgentCnxnProxy, _: Connection, dataToDelete))
  }

  protected def handleDeleteDataByConnectionFetch(cnxn: AgentCnxnProxy, conn: Connection, dataToDelete: Data) =
  {
    delete(_dbQ, conn.writeCnxn, dataToDelete.toStoreKey)
  }

  def updateData(cnxn: AgentCnxnProxy, newData: Data, oldData: Data)
  {
    if (newData != null)
      safeDelete(cnxn, oldData, Some(newData))
    else
      safeDelete(cnxn, oldData, None)

    //TODO: Issue 49
    Thread.sleep(350)
    store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[ Data ](newData))
  }

  def updateDataBySearch [T<:Data](cnxn: AgentCnxnProxy, search: T, newData: Data)
  {
    //this will delete ALL occurrences of the specified data in the search object
    if (newData != null)
      deleteDataBySearch(cnxn, search, Some(newData))
    else
      deleteDataBySearch(cnxn, search, None)

    //TODO: Issue 49
    Thread.sleep(350)
    store(_dbQ, cnxn, newData.toStoreKey, Serializer.serialize[ Data ](newData))
  }

  def deleteDataBySearch [T<:Data](cnxn: AgentCnxnProxy, search: T, newData: Option[ Data ]) : Unit =
  {
    fetchList[ Data ](_dbQ, cnxn, search.toSearchKey, handleDeleteAfterFetch(_: AgentCnxnProxy, _: List[ Data ], newData))
  }

  //could use a notion of retries if it isn't safe to delete
  protected def handleDeleteAfterFetch(cnxn: AgentCnxnProxy, dataToDelete: List[ Data ], newData: Option[ Data ])
  {
    dataToDelete.map(x => safeDelete(cnxn, x, newData))
  }

  //exception to the convention of newData, oldData
  protected def safeDelete(cnxn: AgentCnxnProxy, dataToDelete: Data, dataToPreserve: Option[ Data ]) =
  {
    if ( dataToDelete != null ) {

      //this check prevents the race condition occurring where the new data is saved before the fetch is finished
      dataToPreserve match {
        case None => {
          delete(_dbQ, cnxn, dataToDelete.toStoreKey)
        }
        case Some(x) if x != dataToDelete => {
          delete(_dbQ, cnxn, dataToDelete.toStoreKey)
        }
        case _ => {}
      }
    }
  }

}