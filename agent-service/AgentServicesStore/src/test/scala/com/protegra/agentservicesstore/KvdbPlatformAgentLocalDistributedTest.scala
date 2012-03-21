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
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.AgentTS.mTT._

import com.protegra.agentservicesstore._
import com.biosimilarity.lift.lib.moniker._

class KvdbPlatformAgentLocalDistributedTest
  extends JUnit4(KvdbPlatformAgentLocalDistributedTestSpecs)

object KvdbPlatformAgentLocalDistributedTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentLocalDistributedTestSpecs)

object KvdbPlatformAgentLocalDistributedTestSpecs extends KvdbPlatformAgentBase
with RabbitTestSetup
with Timeouts
{
  val sourceAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
  val acquaintanceAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_UI_PRIVATE)

  val pairedWriter = createJunction(sourceAddress, List(acquaintanceAddress))
  val pairedReader = createJunction(acquaintanceAddress, List(sourceAddress))
//  pairedWriter.agentTwistedPairs
//  pairedReader.agentTwistedPairs

  val timeoutBetween = TIMEOUT_LONG
  //issues with race conditions in tests, 5 fail but shouldn't
//  testMessaging(pairedWriter, pairedReader)
  testWildcardWithPut(pairedWriter, pairedReader)
  testWildcardWithStore(pairedWriter, pairedReader)
  testWildcardWithCursor(pairedWriter, pairedReader)
  testWildcardWithCursorBefore(pairedWriter, pairedReader)

}