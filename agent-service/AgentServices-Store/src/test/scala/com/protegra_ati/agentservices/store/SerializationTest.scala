package com.protegra_ati.agentservices.store

// -*- mode: Scala;-*-
// Filename:    SerializationTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

import org.specs2.runner._
import org.junit.runner._

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

import scala.util.continuations._

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._


import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.lib.moniker._
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
import java.util.{HashMap, UUID}
import org.specs2.mutable._
import biz.source_code.base64Coder.Base64Coder
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import java.net.URI

class SerializationTest extends SpecificationWithJUnit
with SpecsKVDBHelpers
with Timeouts
with RabbitTestSetup
{

   val timeoutBetween = 0

  "Serialize" should {
    "cnxn" in {
      val cnxn = new acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
      oos.writeObject(cnxn)
      oos.close()
      println(new String(Base64Coder.encode(baos.toByteArray())))
    }

    "empty hashmap" in {
      val map = new HashMap[ acT.AgentCnxn, String ]()
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
      oos.writeObject(map)
      oos.close()
      println(new String(Base64Coder.encode(baos.toByteArray())))
    }

    "full hashmap" in {
      val cnxnPartition = new HashMap[ acT.AgentCnxn, String ]()
      val cnxn = new acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
      cnxnPartition.put(cnxn, "test1")
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
      oos.writeObject(cnxnPartition)
      oos.close()
      println(new String(Base64Coder.encode(baos.toByteArray())))
    }

    "junction" in {
      val writer = createNode("127.0.0.1".toURI, List[ URI ]())
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
      oos.writeObject(writer)
      oos.close()
      println(new String(Base64Coder.encode(baos.toByteArray())))
    }

    "full stream" in {
      val stream = Stream(1)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
      oos.writeObject(stream)
      oos.close()
      1 must be_==(1)
    }

    //expected to fail
//    "empty stream" in {
//      val stream = Stream()
//      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
//      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
//      oos.writeObject(stream)
//      oos.close()
//    }
//
//    "junction with get" in {
//      skipped("is this really a problem?")
//      val writer = createNode("127.0.0.1".toURI, List[ URI ]())
//      val cnxn = new acT.AgentCnxn("localhost".toURI, "none", "localhost2".toURI)
//
//      val keyPrivate = "channelPrivate(_)"
//      println("get")
//      reset {
//        for ( e <- writer.get(cnxn)(keyPrivate.toLabel) ) {
//            println("listen received - " + e.toString)
//        }
//      }
//
//      //sleep for clean output
//      Thread.sleep(5000)
//
//      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
//      val oos: ObjectOutputStream = new ObjectOutputStream(baos)
//      oos.writeObject(writer)
//      oos.close()
//      println(new String(Base64Coder.encode(baos.toByteArray())))
//    }
  }

}