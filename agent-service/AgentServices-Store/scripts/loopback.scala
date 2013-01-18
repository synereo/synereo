// -*- mode: Scala;-*- 
// Filename:    ping-pong-setup.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 18 19:23:06 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.test._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store.xml.datasets._
import com.biosimilarity.lift.lib._
import scala.util.continuations._ 
import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._
import java.net.URI
import java.util.UUID
import CCLDSL._
import AgentTS._
import AgentTS.acT._
import AgentTS.mTT._
import com.biosimilarity.lift.lib.SpecialKURIDefaults._
//import com.protegra_ati.agentservices.store.extensions.StringExtensions._
//import com.biosimilarity.lift.lib.AgentURIDefaults._

//scala> val pimgJunq = ptToPt( "10.0.1.9", "10.0.1.5" )
//val pimgJunq = ptToPt( "10.0.1.9", "10.0.1.5" )
//val pimgJunq = ptToPt( "10.0.1.5", "10.0.1.9" )
//pimgJunq: com.protegra_ati.agentservices.store.AgentTS.PartitionedStringMGJ = agent://10.0.1.9/invitation# -> List(agent://10.0.1.5/invitation#)

//scala> val atps = pimgJunq.agentTwistedPairs
//val atps = pimgJunq.agentTwistedPairs
//atps: scala.collection.mutable.Map[java.net.URI,com.protegra_ati.agentservices.store.AgentTS.SemiMonadicAgentJSONAMQPTwistedPair[String]] = Map((agent://10.0.1.5/invitation#,com.biosimilarity.lift.model.store.MonadicDTSMsgScope$SMAJATwistedPair@6a7c8bd))

object exchangeConversions {
  implicit def toPattern(
    s : String
  ) : CnxnCtxtLabel[String,String,String] with Factual =
    CXQ.fromCaseClassInstanceString(
      s
    ).getOrElse(
      null
    ).asInstanceOf[CnxnCtxtLabel[String,String,String] with Factual]

  implicit def toValue( s : String ) : mTT.Resource = mTT.Ground( s )
}

object AUUIDVendor extends UUIDOps 

object level1Resources {
  import AUUIDVendor._
  val cnxn1 = new AgentCnxn("src", getUUID() + "", "trgt")
  val cnxn2 = new AgentCnxn("src", getUUID() + "", "trgt")

  val srcName = new URI( "agent", "localhost", "/" + getUUID(), "" )
  val trgtName = new URI( "agent", "localhost", "/" + getUUID(), "" )

  val lbl1 = "contentChannel(\"email\")"
  val lbl2 = "contactMeUsing(\"email\")"
  val value1 = "for the get Jason.klassen@protegra.com"
  val value2 = "for the win Mikail.Mauz@gmail.com"
}

object level2Resources {
  import AUUIDVendor._
  import level1Resources._
  val pJ1 = ptToPts( srcName, List( trgtName ) )
  val pJ2 = ptToPts( trgtName, List( srcName ) )
}
