// -*- mode: Scala;-*- 
// Filename:    alarm-me.scala 
// Authors:     lgm                                                    
// Creation:    Mon May 14 20:07:52 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib.alarm._
import com.biosimilarity.lift.lib.alarm.usage._
import CollectDPutValUseCase._
import PersistedMonadicKVDBCollectD._
import Being._
import com.biosimilarity.lift.model.store._
import CnxnConversionStringScope._

import scala.util.continuations._

import org.basex.core._
import org.basex.core.cmd.Open
import org.basex.core.cmd.Add
import org.basex.core.cmd.CreateDB

object SoYouThinkYouCanTest {
  def setup() : PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  = {
    PersistedMonadicKVDBCollectD.loadData()
    val recordsFileName = 
      PersistedMonadicKVDBCollectD.importData()( 0 )
    val recordsFileNameRoot = recordsFileName.replace( ".xml", "" )
    val node = kvdb( "/" + ( recordsFileNameRoot ) )
    val clientSession = node.cache.clientSessionFromConfig
    val currentDir = new java.io.File(".").getAbsolutePath()
    val recordsFullFileName = currentDir.replace( "/.", "/" + recordsFileName )
    clientSession.execute( new CreateDB( recordsFileNameRoot ) )
    clientSession.execute( new Add( recordsFullFileName ) )
    node
  }
  def go( kvdb : PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] ) = {
    val pvT1 = partialCaseClassDerivative( pvOne, List( ( "time", "t" ), ( "host", "host" ) ) )
    reset { for( rsrc <- kvdb.get( pvT1 ) ) { println( "received: " + rsrc ) } }
  }
}
