package com.protegra_ati.agentservices.store

import java.util.UUID

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.Being.AgentKVDBNode
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.{PersistedKVDBNodeRequest, PersistedKVDBNodeResponse}
import com.protegra_ati.agentservices.store.mongo.usage._
import com.protegra_ati.agentservices.store.test.{RabbitTestSetup, Timeouts}
import org.scalatest.{MustMatchers, WordSpec}

class AgentKVDBNodeConfigurationTest extends WordSpec with MustMatchers with Timeouts with RabbitTestSetup {

  val timeoutBetween = 0

  "configFileNameOpt" should {

    val sourceAddress = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

    "work if None on default port" in {
      val configFileName: Option[String] = None
      val space: AgentUseCase            = AgentUseCase(configFileName)
      val node: AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] =
        space.createNode(sourceAddress, List(), configFileName)

      //// to debug code path
      // val cnxn = new AgentCnxn(( "self" + UUID.randomUUID.toString ).toURI, "", ( "self" + UUID.randomUUID.toString ).toURI);
      // val lbl = "content(\"email\")".toLabel
      // node.store(cnxn)(lbl, Ground("defaultPort"))

      val name: Option[String] = node.configFileName
      val port: String         = node.configurationFromFile.getOrElse("dbPort", "")
      val dbPort: String       = node.cache.dbPort

      name must ===(configFileName)
      port must ===("1984") // see AppDefaults.scala
      dbPort must ===("27017")
    }

    // TODO: add configuration file for this test
    "work if found on configured port" in {
      val configFileName = None
      val space          = AgentUseCase(configFileName)
      val node           = space.createNode(sourceAddress, List(), configFileName)

      node.configFileName must ===(configFileName)
      node.configurationFromFile.getOrElse("dbPort", "") must ===("1984")
      node.cache.dbHost must ===("127.0.0.1")
      node.cache.dbPort must ===("27017")
    }

    "save in 1985" ignore {
      val configFileName = Some("db_test_1985.conf")
      val space          = AgentUseCase(configFileName)
      val node           = space.createNode(sourceAddress, List(), configFileName)

      val cnxn = AgentCnxn(("self" + UUID.randomUUID.toString).toURI, "", ("self" + UUID.randomUUID.toString).toURI)
      val lbl  = "content(\"email\")".toLabel
      node.store(cnxn)(lbl, Ground("configuredPort"))
      1 must ===(1)
    }
  }
}
