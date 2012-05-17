// -*- mode: Scala;-*- 
// Filename:    ImportFromAMQP.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 19 19:21:52 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.bulk

import com.biosimilarity.lift.lib._

import net.liftweb.json._

import scala.concurrent.{Channel => Chan, _}
import scala.util.continuations._
import scala.xml._
import scala.xml.XML._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait BulkCollectDImport extends UUIDOps {
  case class UUIDWrapper( youyouid : String )
  // """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """

  def supplyEntries( host : String, queue : String, numOfEntries : Int ) : Unit = {
    // create an AMQP scope
    val collectDAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
    val collectDQM =
      new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	host,
	queue
      )
    // get an empty queue
    val collectDQ = collectDQM.zero[String]    
    for( i <- 1 to numOfEntries ) {
      val entry = """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """
      collectDQ ! entry
    }
  }
  def handleEntry( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
    for(
      JObject( fvs ) <- json \\ "putval" ;
      JArray( valueArray ) <- json \\ "values" ;
      JArray( dstypes ) <- json \\ "dstypes" ;
      JArray( dsnames ) <- json \\ "dsnames" ;
      JDouble( time ) <- json \\ "time" ;
      JDouble( interval ) <- json \\ "interval" ; 
      JString( host ) <- json \\ "host" ; 
      JString( plugin ) <- json \\ "plugin" ;
      JString( plugin_instance ) <- json \\ "plugin_instance" ;
      JString( cdtype ) <- json \\ "type" ;
      JString( type_instance ) <- json \\ "type_instance"
    ) {
      val gvStr =
	new XStream(
	  new JettisonMappedXmlDriver()
	).toXML( UUIDWrapper( ( getUUID + "" ) ) ).replace(
	  "com.biosimilarity.lift.lib.bulk.BulkCollectDImport$UUIDWrapper",
	  "com.biosimilarity.lift.model.store.MonadicTermTypes$Ground"
	).replace(
	  "com.biosimilarity.lift.lib.bulk.usage.BulkCollectDImporter",
	  "com.biosimilarity.lift.model.store.usage.PersistedMonadicKVDBCollectD$TheMTT"
	).replace(
	  "youyouid",
	  "v"
	)

      acc +=
      // <record>
// 	<putval>
// 	  <values>{for( JInt( v ) <- valueArray ) yield {<int>{v}</int>}}</values>
// 	  <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
// 	  <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
//           <time>{time}</time>
// 	  <interval>{interval}</interval>
//           <host>{host}</host>
//           <plugin>{plugin}</plugin>
//           <plugin_instance>{plugin_instance}</plugin_instance>
//           <type>{cdtype}</type>
//           <type_instance>{type_instance}</type_instance>
// 	</putval>
// 	<string>{getUUID + ""}</string>
//       </record>            
      <record>
	<comBiosimilarityLiftLibAlarmPutVal>
	  <values>{for( JInt( v ) <- valueArray ) yield {<string>{v}</string>}}</values>
	  <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
	  <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
          <time>{<string>{time}</string>}</time>
	  <interval>{<string>{interval}</string>}</interval>
          <host>{<string>{host}</string>}</host>
          <plugin>{<string>{plugin}</string>}</plugin>
          <plugin_instance>{<string>{plugin_instance}</string>}</plugin_instance>
          <type>{<string>{cdtype}</string>}</type>
          <type_instance>{<string>{type_instance}</string>}</type_instance>
	</comBiosimilarityLiftLibAlarmPutVal>
	<string>{gvStr}</string>
      </record>
    }
    acc
  }
  def readEntries( host : String, queue : String, file : String, dbChunk : Int ) : ListBuffer[String] = {
    // create an AMQP scope
    val collectDAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
    val collectDQM =
      new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	host,
	queue
      )
    // get an empty queue
    val collectDQ = collectDQM.zero[String]    

    val acc = new ListBuffer[Elem]()
    val fileNames = new ListBuffer[String]()      
    val lock = new Lock()
    
    for ( entry <- collectDQM( collectDQ ) ) {
      handleEntry( parse( entry ), acc )
      lock.acquire
      if ( acc.size > dbChunk ) {
	val db = <records>{acc.toList}</records>
	val recordsFileName = ( file + getUUID + ".xml" )
	scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )
	fileNames += recordsFileName
	acc.clear
      }
      lock.release
    }    
    fileNames
  }
}

package usage {
  object BulkCollectDImporter extends BulkCollectDImport {
    def loadData() : Unit = {
      supplyEntries( "localhost", "collectDSample", 1000 )
    }
    def importData() : Unit = {
      readEntries( "localhost", "collectDSample", "collectDImport", 500 )
    }
  }
}
