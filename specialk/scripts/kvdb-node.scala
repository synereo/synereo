// -*- mode: Scala;-*- 
// Filename:    kvdb-node.scala 
// Authors:     lgm                                                    
// Creation:    Tue Mar  6 04:25:11 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.util.continuations._ 

import com.biosimilarity.lift.model.store.usage._
import com.biosimilarity.lift.model.store.usage.MolecularUseCase._

object CookSoup {
  import java.net.InetAddress
  def localIP : String = {
    InetAddress.getLocalHost().getHostAddress
  }
  def play() : Unit = {
    val Right( ( client, server ) ) =
      setup( "localhost", 5672, "localhost", 5672 )( true )
    runClient( client )
    runServer( server )
  }
  def playClient( serverIP : String ) = {
    val Left( client ) =
      setup( localIP, 5672, serverIP, 5672 )( false )
    runClient( client )
    client
  }
  def playServer( clientIP : String ) = {
    val Left( server ) =
      setup( localIP, 5672, clientIP, 5672 )( false )
    runServer( server )
    server
  }
}
