/*
 This should only interface with the UI and the AgentHostStorePA for db access, not visible to the public network
 */

package com.protegra_ati.agentservices.core.platformagents

import com.protegra_ati.agentservices.core.platformagents.behaviors._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages.content._
import java.net.URI
import java.util.UUID
import net.lag.configgy._
import java.net.URI

class MockPlatformAgent extends BasePlatformAgent
with Serializable
with Storage
with Private
{
  override def loadQueues()
  {
    loadStorageQueue()
    loadPrivateQueue()
  }

  def initForTest(privateAddress: URI, privateAcquaintanceAddresses: List[ URI ], dbAddress: URI, id: UUID) =
  {
    initPrivate(privateAddress, privateAcquaintanceAddresses)
    initDb(dbAddress)

    super.initForTest(id)
  }

  def init(configUtil: Config) =
  {

  }

  override def startListening() =
  {
    //do nothing call listen explicitly in test
  }

}


