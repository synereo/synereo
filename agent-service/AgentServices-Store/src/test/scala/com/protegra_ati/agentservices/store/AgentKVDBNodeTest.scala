package com.protegra_ati.agentservices.store

import org.specs2.runner._
import org.junit.runner._

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

import scala.util.continuations._


import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._


import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.lib.moniker._
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
import java.util.{HashMap, UUID}
import org.specs2.mutable._
import biz.source_code.base64Coder.Base64Coder
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import java.net.URI

class AgentKVDBNodeTest extends SpecificationWithJUnit
with SpecsKVDBHelpers
with Timeouts
with RabbitTestSetup
{
   val timeoutBetween = 0

  "configFileNameOpt" should {
    val sourceAddress = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

    "work if None on default port" in {
      val configFileName = None
      val space = AgentUseCase(configFileName)
      val node = space.createNode(sourceAddress, List(), configFileName)

      node.configFileName must be_==(configFileName)
      node.configurationFromFile.get( "dbPort" ).getOrElse("") must be_==("1984")
      val clientSession = node.cache.dbPort must be_==("1984")
     //to debug code path
      val cnxn = new AgentCnxn(( "self" + UUID.randomUUID.toString ).toURI, "", ( "self" + UUID.randomUUID.toString ).toURI);
      val lbl = "content(\"email\")".toLabel
      node.store(cnxn)(lbl, Ground("defaultPort"))
    }

    "work if found on configured port" in {
      val configFileName = Some("db_test_1985.conf")
      val space = AgentUseCase(configFileName)
      val node = space.createNode(sourceAddress, List(), configFileName)

      node.configFileName must be_==(configFileName)
      node.configurationFromFile.get( "dbPort" ).getOrElse("") must be_==("1985")
      node.cache.dbPort must be_==("1985")
    }

    "save in 1985" in {
      val configFileName = Some("db_test_1985.conf")
      val space = AgentUseCase(configFileName)
      val node = space.createNode(sourceAddress, List(), configFileName)

      //to debug code path
      val cnxn = new AgentCnxn(( "self" + UUID.randomUUID.toString ).toURI, "", ( "self" + UUID.randomUUID.toString ).toURI);
      val lbl = "content(\"email\")".toLabel
      node.store(cnxn)(lbl, Ground("configuredPort"))
      1 must be_==(1)
    }


  }

}