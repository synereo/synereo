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
  lazy val dsp = new KVDBJSONAPIDispatcher( "localhost", "kvdb" )

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
  val msgBody = 
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
  lazy val justification = 
    "{ " + "\"response\"" + " : " + "null" + " }"
  lazy val allHdrs = 
    (
      "["
      + to + "," + from + "," + msgId + "," + flowId + ","
      + justification
      + "]"
    );
  lazy val msgHdrsBody = 
    makeMsg( allHdrs, msgBody );

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

  def !() : Unit = {
    val srcScope : AMQPNodeJSScope = new AMQPNodeJSStdScope()
    val srcQM = new srcScope.AMQPNodeJSQueueM( "localhost", "kvdb" )
    val srcQ = srcQM.zeroJSON
    
    srcQ ! msgHdrsBody
  }
}
