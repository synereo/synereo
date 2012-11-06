package com.protegra_ati.agentservices.core.platformagents.behaviors

/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

import org.specs._

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import moniker._
import com.protegra_ati.agentservices.core.messages._
import org.specs.runner._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._

import platformagents._
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.Serializer


class ResultStorageTest
  extends JUnit4(ResultStorageTestSpecs)

object ResultStorageTestSpecsRunner
  extends ConsoleRunner(ResultStorageTestSpecs)

object ResultStorageTestSpecs extends Specification
with RabbitTestSetup
with Timeouts
with SpecsPAHelpers
{
  AgentHostCombinedBase.setup(this)

  val pa = AgentHostCombinedBase.storeRef

  "getList" should {

    "find results" in {
      val parentId1 = UUID.randomUUID().toString
      val resultKey1 = pa.getResultKey(parentId1)
      val parentId2 = UUID.randomUUID().toString
      val resultKey2 = pa.getResultKey(parentId2)

      val expected: List[String] = "test1" :: "test2" :: "test3" :: Nil

      pa.put(pa._resultQ, pa._cnxnResult, resultKey1, Serializer.serialize[ String ](expected(0))  )
      pa.put(pa._resultQ, pa._cnxnResult, resultKey1, Serializer.serialize[ String ](expected(1))  )
      pa.put(pa._resultQ, pa._cnxnResult, resultKey1, Serializer.serialize[ String ](expected(2))  )
      pa.put(pa._resultQ, pa._cnxnResult, resultKey2, Serializer.serialize[ String ]("test11")  )
      pa.put(pa._resultQ, pa._cnxnResult, resultKey2,  Serializer.serialize[ String ]("test12")  )

      //if intermittent put this back in
//      Thread.sleep(TIMEOUT_LONG)
      val resultSearchKey1 = pa.getResultSearchKey(parentId1)
      pa.getList[String](pa._resultQ, pa._cnxnResult, resultSearchKey1, handleGetList(_: AgentCnxnProxy, _:List[String], expected))
    }

    def handleGetList(cnxn: AgentCnxnProxy, data: List[String], expected: List[String]) =
    {
      val allResults = data ::: expected
      allResults.distinct.size must be_==(expected.size)
    }

  }
}
