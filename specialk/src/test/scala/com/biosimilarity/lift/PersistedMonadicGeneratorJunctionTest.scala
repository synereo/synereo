package com.biosimilarity.lift.test.store

import com.biosimilarity.lift.lib.extensions.StringExtensions._
import com.biosimilarity.lift.lib.extensions.URMExtensions._

import java.util.UUID
import org.junit._
import com.biosimilarity.lift.model.store.usage.PersistedMonadicTS._
import Being._

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.biosimilarity.lift.lib.moniker._
import scala.util.continuations._
import scala.concurrent.ops._

class PersistedMonadicGeneratorJunctionTest
  extends JUnit4(PersistedMonadicGeneratorJunctionTestSpecs)

object PersistedMonadicGeneratorJunctionTestSpecsRunner
  extends ConsoleRunner(PersistedMonadicGeneratorJunctionTestSpecs)

object PersistedMonadicGeneratorJunctionTestSpecs extends Specification
{

  val TIMEOUT_EVENTUALLY = new Duration(1500)

  val RABBIT_PORT_WRITER = 5672
  val RABBIT_PORT_READER = 6000
  val RABBIT_PORT_READER2 = 6001
  val RABBIT_PORT_UNRELATED = 4000

  "PersistedMonadicGeneratorJunction" should {
    var found = false;

   val dbWriterReader = "KVDB-WriterReader" + UUID.randomUUID().toString

    val writer_location = "localhost".toURM.withPort(RABBIT_PORT_WRITER)
    val reader_location = "localhost".toURM.withPort(RABBIT_PORT_READER)
    val reader2_location = "localhost".toURM.withPort(RABBIT_PORT_READER2)
    val unrelated_location = "localhost".toURM.withPort(RABBIT_PORT_UNRELATED)

    "retrieve between two queues" in {
      RetrieveBetweenTwoQueues() //success
      RetrieveBetweenTwoQueuesUnrelatedQueueNoAcquaintances() //success
      RetrieveBetweenTwoQueuesUnrelatedQueueWithAcquaintances() //success
      RetrieveBetweenTwoQueuesWithMultipleAcquaintances() //success
      RetrieveBetweenTwoQueuesUnrelatedQueueWithAcquaintancesNoGet() //success
    }

    def RetrieveBetweenTwoQueues() =
    {
      val writer_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, writer_location, Seq[ URM ](reader_location))
      writer_privateQ.agentTwistedPairs

      val reader_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader_location, Seq[ URM ](writer_location))
      reader_privateQ.agentTwistedPairs

      getPut(reader_privateQ, writer_privateQ)
    }

    def RetrieveBetweenTwoQueuesUnrelatedQueueNoAcquaintances() =
    {
      val writer_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, writer_location, Seq[ URM ](reader_location))
      writer_privateQ.agentTwistedPairs

      val reader_msgQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader2_location, Seq[ URM ]())
      reader_msgQ.agentTwistedPairs
      val reader_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader_location, Seq[ URM ](writer_location))
      reader_privateQ.agentTwistedPairs

      val keyPublic = "channelPublic(_)"
      reset {
        for ( e <- reader_msgQ.get(keyPublic.toLabel) ) {}
      }

      getPut(reader_privateQ, writer_privateQ)
    }

    def RetrieveBetweenTwoQueuesUnrelatedQueueWithAcquaintances() =
    {
      val writer_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, writer_location, Seq[ URM ](reader_location))
      writer_privateQ.agentTwistedPairs

      val reader_msgQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader2_location, Seq[ URM ](unrelated_location))
      reader_msgQ.agentTwistedPairs
      val reader_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader_location, Seq[ URM ](writer_location))
      reader_privateQ.agentTwistedPairs

      val keyPublic = "channelPublic(_)"
      reset {
        for ( e <- reader_msgQ.get(keyPublic.toLabel) ) {}
      }

      getPut(reader_privateQ, writer_privateQ)
    }

    def RetrieveBetweenTwoQueuesWithMultipleAcquaintances() =
    {
      val writer_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, writer_location, Seq[ URM ](reader_location))
      writer_privateQ.agentTwistedPairs

      val reader_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader_location, Seq[ URM ](writer_location, unrelated_location))
      reader_privateQ.agentTwistedPairs

      val keyPublic = "channelPublic(_)"
      reset {
        for ( e <- reader_privateQ.get(keyPublic.toLabel) ) {}
      }

      getPut(reader_privateQ, writer_privateQ)
    }

    def RetrieveBetweenTwoQueuesUnrelatedQueueWithAcquaintancesNoGet() =
    {
      val writer_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, writer_location, Seq[ URM ](reader_location))
      writer_privateQ.agentTwistedPairs

      val reader_msgQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader2_location, Seq[ URM ](unrelated_location))
      reader_msgQ.agentTwistedPairs
      val reader_privateQ: PersistedStringMGJ = new PersistedStringMGJ(dbWriterReader, reader_location, Seq[ URM ](writer_location))
      reader_privateQ.agentTwistedPairs

      getPut(reader_privateQ, writer_privateQ)
    }

    def getPut(reader: PersistedStringMGJ, writer: PersistedStringMGJ) =
    {
      val keyPrivate = "channelPrivate(_)"
      reset {
        for ( e <- reader.get(keyPrivate.toLabel) ) {
          //removing e!= None causes it to work
          if ( e != None ) {
            found = true;
          }
          else {
            println("listen received - none")
          }
        }
      }

      val keyMsg = "channelPrivate(\"" + UUID.randomUUID() + "\")"
      val value = "test"
      reset {writer.put(keyMsg.toLabel, mTT.Ground(value))}

      found must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }


}
