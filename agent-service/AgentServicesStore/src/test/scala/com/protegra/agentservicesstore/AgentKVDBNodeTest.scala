package com.protegra.agentservicesstore

import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._

import scala.util.continuations._


import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._
import Being.AgentKVDBNodeFactory

import com.protegra.agentservicesstore._
import com.biosimilarity.lift.lib.moniker._
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
import java.util.{HashMap, UUID}
import org.specs.Specification
import biz.source_code.base64Coder.Base64Coder
import com.protegra.agentservicesstore.extensions.URIExtensions._
import java.net.URI

class AgentKVDBNodeTest
  extends JUnit4(AgentKVDBNodeTestSpecs)

object AgentKVDBNodeTestSpecsRunner
  extends ConsoleRunner(AgentKVDBNodeTestSpecs)

object AgentKVDBNodeTestSpecs extends Specification
with SpecsKVDBHelpers
with Timeouts
with RabbitTestSetup
{
   val timeoutBetween = 0

  "configFileNameOpt" should {
    val sourceAddress = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

//    "work if None on default port" in {
//      val configFileName = None
//      val node = AgentKVDBNodeFactory.ptToMany(sourceAddress, List())(None)
//
//      node.configFileName must be_==(configFileName)
//      node.configurationFromFile.get( "dbPort" ).getOrElse("") must be_==("1984")
//      val clientSession = node.cache.dbPort must be_==("1984")
//    }

    "work if found on configured port" in {
      val configFileName = Some("db_test_1985.conf")
      val node = AgentKVDBNodeFactory.ptToMany(sourceAddress, List())(configFileName)

      node.configFileName must be_==(configFileName)
      node.configurationFromFile.get( "dbPort" ).getOrElse("") must be_==("1985")
      node.cache.dbPort must be_==("1985")

      //to debug code path
      //val cnxn = new AgentCnxn(( "self" + UUID.randomUUID.toString ).toURI, "", ( "self" + UUID.randomUUID.toString ).toURI);
      //val lbl = "content(\"email\")".toLabel
      //node.store(cnxn)(lbl, Ground("configuredPort"))
    }

    //throw configgy exception when not found

  }

}