// -*- mode: Scala;-*- 
// Filename:    agent.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jun  4 18:31:32 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import CnxnConversionStringScope._
import com.biosimilarity.lift.lib._
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.protegra_ati.agentservices.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.usage._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._

object Instrument extends Serializable {
  import java.util.UUID
  implicit val pvTwo =
    PutVal(
      List( 558816, 43649770 ),
      List( "derive", "derive" ),
      List( "rx", "tx" ),
      1334349094.633,
      12.000,
      "server-75530.localdomain",
      "interface",
      "eth0",
      "if_octets",
      ""
    )

  implicit val configurationFileName : String = "stressTest.conf"

  implicit def mkUseCaseFromFileName( implicit fileName : String ) : AgentUseCase = {
    AgentUseCase( Some( fileName ) )
  }

  implicit def testDataStream( implicit agentUseCase : AgentUseCase ) : Stream[String] = {
    agentUseCase.tStream(
      UUID.randomUUID + ""
    )( ( s : String ) => UUID.randomUUID + "" )
  }

  implicit def testDataHandler : Option[mTT.Resource] => Unit = {
    ( orsrc : Option[mTT.Resource] ) => {
      orsrc match {
	case Some( rsrc ) => {
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>" ) 
	  println( "received: " + rsrc ) 
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>" ) 
	}
	case None => {
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>" ) 
	  println( "No resource received, yet." )
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>" ) 
	}
      }
    }
  }

  def getTest( implicit agentUseCase : AgentUseCase ) : Unit = {
    import agentUseCase._
    val tc =
      TestConfiguration[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
	StdTestConfigurationGenerator,
	pvTwo,
	Some( List( ( "time", "t" ), ( "host", "host" ) ) ),
	Some(
	  configureTest[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( 
	    StdTestConfigurationGenerator
	  )
	)
      )
    sporeGet[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( tc )( testDataHandler )
  }
  def putTest( implicit agentUseCase : AgentUseCase ) : Unit = {
    import agentUseCase._
    val tc =
      TestConfiguration[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
	StdTestConfigurationGenerator,
	pvTwo,
	Some( List( ( "time", "t" ), ( "host", "host" ) ) ),
	Some(
	  configureTest[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( 
	    StdTestConfigurationGenerator
	  )
	)
      )
    sporePut[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( tc )( testDataStream )
  }
}
