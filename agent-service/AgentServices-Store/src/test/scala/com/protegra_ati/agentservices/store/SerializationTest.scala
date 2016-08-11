// -*- mode: Scala;-*-
// Filename:    SerializationTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.net.URI

import biz.source_code.base64Coder.Base64Coder
import com.biosimilarity.lift.lib.BasicLogService
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.test.{RabbitTestSetup, ScalaTestKVDBHelpers, Timeouts}
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.continuations._

class SerializationTest
    extends WordSpec
    with MustMatchers
    with Eventually
    with IntegrationPatience
    with ScalaTestKVDBHelpers
    with Timeouts
    with RabbitTestSetup {

  val timeoutBetween = 0

  "Serialize" should {
    "cnxn" in {
      val cnxn                        = acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
      oos.writeObject(cnxn)
      oos.close()
      BasicLogService.tweet(new String(Base64Coder.encode(baos.toByteArray)))
    }

    "empty hashmap" in {
      val map                         = new java.util.HashMap[acT.AgentCnxn, String]()
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
      oos.writeObject(map)
      oos.close()
      BasicLogService.tweet(new String(Base64Coder.encode(baos.toByteArray)))
    }

    "full hashmap" in {
      val cnxnPartition = new java.util.HashMap[acT.AgentCnxn, String]()
      val cnxn          = acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
      cnxnPartition.put(cnxn, "test1")
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
      oos.writeObject(cnxnPartition)
      oos.close()
      BasicLogService.tweet(new String(Base64Coder.encode(baos.toByteArray)))
    }

    "junction" in {
      val writer                      = createNode("127.0.0.1".toURI, List[URI]())
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
      oos.writeObject(writer)
      oos.close()
      BasicLogService.tweet(new String(Base64Coder.encode(baos.toByteArray)))
    }

    "full stream" in {
      val stream                      = Stream(1)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
      oos.writeObject(stream)
      oos.close()
      1 must ===(1)
    }

    "expected to fail" should {

      "empty stream" ignore {
        val stream = Stream()
        val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos: ObjectOutputStream = new ObjectOutputStream(baos)
        oos.writeObject(stream)
        oos.close()
      }

      "junction with get" ignore {
        val writer = createNode("127.0.0.1".toURI, List[ URI ]())
        val cnxn = new acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
        val keyPrivate = "channelPrivate(_)"
        BasicLogService.tweet("get")
        reset {
          for ( e <- writer.get(cnxn)(keyPrivate.toLabel) ) {
            BasicLogService.tweet("listen received - " + e.toString)
          }
        }
        // sleep for clean output
        Thread.sleep(5000)
        val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos: ObjectOutputStream = new ObjectOutputStream(baos)
        oos.writeObject(writer)
        oos.close()
        BasicLogService.tweet(new String(Base64Coder.encode(baos.toByteArray())))
      }
    }
  }
}
