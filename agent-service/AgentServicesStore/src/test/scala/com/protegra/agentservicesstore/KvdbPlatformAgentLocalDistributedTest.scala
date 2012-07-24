package com.protegra.agentservicesstore

// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
 
import com.biosimilarity.lift.model.store._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import Being.AgentKVDBNodeFactory

class KvdbPlatformAgentLocalDistributedTest
  extends JUnit4(KvdbPlatformAgentLocalDistributedTestSpecs)

object KvdbPlatformAgentLocalDistributedTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentLocalDistributedTestSpecs)

object KvdbPlatformAgentLocalDistributedTestSpecs extends KvdbPlatformAgentBase
{
  val timeoutBetween = TIMEOUT_LONG

  val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val acquaintanceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

  val pairedWriter = createNode(sourceAddress, List(acquaintanceAddress))
  val pairedReader = createNode(acquaintanceAddress, List(sourceAddress))

  testMessaging(pairedWriter, pairedReader)
  testWildcardWithPut(pairedWriter, pairedReader)
  testWildcardWithStore(pairedWriter, pairedReader)
  testWildcardWithPutAndCursor(pairedWriter, pairedReader)
  testWildcardWithStoreAndCursor(pairedWriter, pairedReader)
//  testWildcardWithCursorBefore(pairedWriter, pairedReader)

}