package com.protegra_ati.agentservices.store

import java.util.UUID

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._
import com.protegra_ati.agentservices.store.test.{RabbitTestSetup, Timeouts}
import org.scalatest.{MustMatchers, WordSpec}

class AgentKVDBNodeTest extends WordSpec with MustMatchers with Timeouts with RabbitTestSetup {

  val timeoutBetween = 0
  val sourceAddress  = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

  "configFileNameOpt" should {

    "work if None on default port" ignore {
      val configFileName = None
      val space          = AgentUseCase(configFileName)
      val node           = space.createNode(sourceAddress, List(), configFileName)
      node.configFileName must ===(configFileName)
      node.configurationFromFile.getOrElse("dbPort", "") must ===("1984")
      val clientSession = node.cache.dbPort must ===("1984")
      //to debug code path
      val cnxn = AgentCnxn(("self" + UUID.randomUUID.toString).toURI, "", ("self" + UUID.randomUUID.toString).toURI)
      val lbl  = "content(\"email\")".toLabel
      node.store(cnxn)(lbl, Ground("defaultPort"))
    }

    "work if found on configured port" ignore {
      val configFileName = Some("db_test_1985.conf")
      val space          = AgentUseCase(configFileName)
      val node           = space.createNode(sourceAddress, List(), configFileName)
      node.configFileName must ===(configFileName)
      node.configurationFromFile.getOrElse("dbPort", "") must ===("1985")
      node.cache.dbPort must ===("1985")
    }

    "save in 1985" ignore {
      val configFileName = Some("db_test_1985.conf")
      val space          = AgentUseCase(configFileName)
      val node           = space.createNode(sourceAddress, List(), configFileName)
      val cnxn           = AgentCnxn(("self" + UUID.randomUUID.toString).toURI, "", ("self" + UUID.randomUUID.toString).toURI)
      val lbl            = """content("email")""".toLabel
      node.store(cnxn)(lbl, Ground("configuredPort"))
      1 must ===(1)
    }
  }
}
