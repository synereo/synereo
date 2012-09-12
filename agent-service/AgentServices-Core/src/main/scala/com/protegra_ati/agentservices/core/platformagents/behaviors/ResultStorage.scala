package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.platformagents.behaviors._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.biosimilarity.lift.lib.moniker._
import net.lag.configgy._
import java.util.UUID
import com.protegra.agentservicesstore.extensions.StringExtensions._
import java.util.concurrent._

trait ResultStorage
{
  self: BasePlatformAgent =>

  var _resultLocation: URM = null
  var _resultQ: PartitionedStringMGJ = null //persistedJunction

  val _resultId = UUID.randomUUID().toString()
  val _cnxnResult = new AgentCnxn(( "ResultDB" + _resultId ).toURI, "", ( "ResultDB" + _resultId ).toURI)

  def initResultDb(configUtil: Config)
  {
    val resultSelfMapKey = "result.self"
    _resultLocation = loadFirstURM(configUtil.getConfigMap(resultSelfMapKey))
  }

  def initResultDb(resultLocation: URM)
  {
    _resultLocation = resultLocation
  }

  def loadResultStorageQueue() =
  {
    _resultQ = new PartitionedStringMGJ(_resultLocation, List(), None)
  }

  //childId for uniqueness, if we just used Id an update would happen
  def getResultKey(id: String): String =
  {
    "result(id(\"" + id + "\"),childId(\"" + UUID.randomUUID() + "\"))"
  }

  def getResultSearchKey(id: String): String =
  {
    "result(id(\"" + id + "\"),_)"
  }

  def storeResult[ T ](partialKey: String, resultToStore: T) =
  {
    //use modified conversationId(with getResultKey-method) as a key
    val storeKey = getResultKey(partialKey)
    put(_resultQ, _cnxnResult, storeKey, Serializer.serialize[ T ](resultToStore))
  }

  def fetchResults[ T ](partialResultSearchKey: String, resultHandler: (AgentCnxn, List[ T ]) => Unit) =
  {
    val resultSearchKey = getResultSearchKey(partialResultSearchKey)
    getList[ T ](_resultQ, _cnxnResult, resultSearchKey, resultHandler(_: AgentCnxn, _: List[ T ]))
  }

}
