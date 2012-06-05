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

import com.protegra.agentservicesstore._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.usage._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
//import com.protegra.agentservicesstore.usage.AgentKVDBScope.Being._
//import com.protegra.agentservicesstore.usage.AgentKVDBScope.Being.AgentKVDBNodeFactory._
import com.protegra.agentservicesstore.usage.AgentUseCase._

object Cookery {
  import java.net.InetAddress
  val cnxnGlobal = new acT.AgentCnxn("Global".toURI, "", "Global".toURI)
  def localIP : String = {
    InetAddress.getLocalHost().getHostAddress
  }
  def play() = {
    val Right( ( client, server ) ) =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( "localhost", 5672, "localhost", 5672 )( true )
    //runClient( client )
    //runServer( server )
    ( client, server )
  }
  def playClient( serverIP : String ) = {
    val Left( client ) =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( localIP, 5672, serverIP, 5672 )( false )
    //runClient( client )
    client
  }
  def playServer( clientIP : String ) = {
    val Left( server ) =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( localIP, 5672, clientIP, 5672 )( false )
    //runServer( server )
    server
  }
}
