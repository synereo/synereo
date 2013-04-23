package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent, AgentHostUIPlatformAgent}
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy

class LauncherPluginSession {
  val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
  var selfAlias: String = "John Smith"
  // TBD - needs to come from elsewhere
  var agentSessionId: UUID = UUID.randomUUID()
  // TBD - needs to come from elsewhere
  var userAgentId: UUID = UUID.randomUUID()
  // TBD - needs to come from elsewhere
  var ui: AgentHostUIPlatformAgent = null
  var store: AgentHostStorePlatformAgent = null
  var selfCnxn: AgentCnxnProxy = null
}

object LauncherPluginSession {
  val session = new LauncherPluginSession()
}


