// -*- mode: Scala;-*- 
// Filename:    monadic-node.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 27 16:04:33 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.usage._
import scala.util.continuations._ 

import FramedMsgDispatcherUseCase._

object LoopbackPlayer {
  def play() : Unit = {
    val Right( ( client, server ) ) =
      setup( "localhost", 5672, "localhost", 5672 )( true )
    runClient( client )
    runServer( server )
  }
}

object TwoDevicePlayer {
  import java.net.InetAddress
  def localIP : String = {
    InetAddress.getLocalHost().getHostAddress
  }
  def playClient( serverIP : String ) : Unit = {
    val Right( ( client, server ) ) =
      setup( localIP, 5672, serverIP, 5672 )( true )
    runClient( client )
  }
  def playServer( clientIP : String ) : Unit = {
    val Right( ( client, server ) ) =
      setup( clientIP, 5672, localIP, 5672 )( true )
    runServer( server )
  }
}
