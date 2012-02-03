// -*- mode: Scala;-*- 
// Filename:    dispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jan 25 17:49:03 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib.kvdbJSON._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import java.net.URI
import java.util.UUID

object Dispatcher {
  // Make a dispatcher
  lazy val dsp =
    new KVDBJSONAPIDispatcher( new URI( "amqp", "localhost", "/kvdb", "" ) )

  lazy val trgtURI = 
    new URI( "agent", "localhost", "/kvdbDispatchStore1", "" )

  lazy val srcURI = 
    new URI( "agent", "localhost", "/kvdbDispatchStore2", "" )  

  def setup() : KVDBJSONAPIDispatcher = {
    // Configure it to serve requests with a "to" header of agent://localhost/kvdbDispatchStore1
    // and a "from" header of agent://localhost/kvdbDispatchStore2
    // and deposit replies to the kvdbReplyQueue in the kvdbReplyExchange
  
    dsp.addSingletonKVDB( trgtURI )
    dsp.addReplyQueue( srcURI, "localhost", "kvdbReply" )
    dsp.serveAPI
    
    dsp
  }
}

object Probe extends UUIDOps {
  lazy val to = "agent://localhost/kvdbDispatchStore1"
  lazy val from = "agent://localhost/kvdbDispatchStore2"
  lazy val msgId : String = getUUID + ""
  lazy val flowId : String = getUUID + ""
  val getMsgBody = 
    (
      "{ "
      + "\"getRequest\" : "
      + "{ "
      +    "\"ask\" : "
      +        "{ " 
      +           "\"node\" : "
      +              "[" 
      +                "{ "
      +                   "\"machine\" : "
      +                      "["
      +                        "\"sl390\""
      +                      "]"
      +                " }, "
      +                "{ "
      +                   "\"os\" : " 
      +                      "["
      +                        "\"Ubuntu\","
      +                        "\"11.04\""
      +                      "]"
      +                " }"
      +              " ]"
      +        " }"
      + " }"
      + " }"
    );

  val putMsgBody = 
    (
      "{ "
      + "\"putRequest\" : "
      + "{ "
      +    "\"tell\" : "
      +       "[ "
      +        "{ " 
      +           "\"node\" : "
      +              "[" 
      +                "{ "
      +                   "\"machine\" : "
      +                      "["
      +                        "\"sl390\""
      +                      "]"
      +                " }, "
      +                "{ "
      +                   "\"os\" : " 
      +                      "["
      +                        "\"Ubuntu\","
      +                        "\"11.04\""
      +                      "]"
      +                " }"
      +              " ]"
      +        " }"
      +           ", "
      +        "\"running\""
      +       " ]"
      + " }"
      + " }"
    );

  lazy val justification = 
    "{ " + "\"response\"" + " : " + "null" + " }"
  lazy val allHdrs = 
    (
      "["
      + to + "," + from + "," + msgId + "," + flowId + ","
      + justification
      + "]"
    );
  lazy val getMsgHdrsBody = 
    makeMsg( allHdrs, getMsgBody );
  lazy val putMsgHdrsBody = 
    makeMsg( allHdrs, putMsgBody );

  def makeMsg( hdrs : String, body : String ) : String = {
    (
      "{ "
      + "\"headers\" : "
      + hdrs
      + ", "
      + "\"body\" : "
      + body
      + " }"
    )
  }

  def !( host : String, exchange : String ) : Unit = {
    val srcScope : AMQPNodeJSScope = new AMQPNodeJSStdScope()
    val srcQM = new srcScope.AMQPNodeJSQueueM( host, exchange )
    val srcQ = srcQM.zeroJSON
    
    srcQ ! putMsgHdrsBody
  }

  def !() : Unit = this ! ( "localhost", "kvdb" )

  def ?( host : String, exchange : String ) : Unit = {
    val srcScope : AMQPNodeJSScope = new AMQPNodeJSStdScope()
    val srcQM = new srcScope.AMQPNodeJSQueueM( host, exchange )
    val srcQ = srcQM.zeroJSON
    
    srcQ ! getMsgHdrsBody    
  }
  
  def ?() : Unit = this ? ( "localhost", "kvdb" )

  def showReplies( host : String, exchange : String ) : Unit = {
    val srcScope : AMQPNodeJSScope = new AMQPNodeJSStdScope()
    val srcQM = new srcScope.AMQPNodeJSQueueM( host, exchange )
    val srcQ = srcQM.zeroJSON

    for( rply <- srcQM( srcQ ) ) {
      println( "received: " + rply )
    }
  }

  def showReplies() : Unit = showReplies( "localhost", "kvdbReply" )
}


