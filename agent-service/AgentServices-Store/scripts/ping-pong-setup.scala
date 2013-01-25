// -*- mode: Scala;-*- 
// Filename:    ping-pong-setup.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 18 19:23:06 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

//scala> import com.protegra_ati.agentservices.store._
import com.protegra_ati.agentservices.store._
//import com.protegra_ati.agentservices.store._

//scala> import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store._
//import com.biosimilarity.lift.model.store._

import com.biosimilarity.lift.model.store.test._

//scala> import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store.xml._
//import com.biosimilarity.lift.model.store.xml._

//scala> import com.biosimilarity.lift.model.store.xml.datasets._
import com.biosimilarity.lift.model.store.xml.datasets._
//import com.biosimilarity.lift.model.store.xml.datasets._

//scala> import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib._
//import com.biosimilarity.lift.lib._

//scala> import scala.util.continuations._ 
import scala.util.continuations._ 
//import scala.util.continuations._

//scala> import org.xmldb.api.base._
import org.xmldb.api.base._
//import org.xmldb.api.base._

//scala> import org.xmldb.api.modules._
import org.xmldb.api.modules._
//import org.xmldb.api.modules._

//scala> import org.xmldb.api._
import org.xmldb.api._
//import org.xmldb.api._

//scala> import java.net.URI
import java.net.URI
//import java.net.URI

//scala> import java.util.UUID
import java.util.UUID
//import java.util.UUID

//scala> import CCLDSL._
import CCLDSL._
//import CCLDSL._

//scala> import AgentTS._
import AgentTS._
//import AgentTS._

//scala> import AgentTS.acT._
import AgentTS.acT._
//import AgentTS.acT._

//scala> import AgentTS.mTT._
import AgentTS.mTT._
//import AgentTS.mTT._

//scala> import com.biosimilarity.lift.lib.AgentURIDefaults._
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

//scala> val cnxn = new AgentCnxn("laptop", "testing", "server")
//val cnxn = new AgentCnxn("laptop", "testing", "server")
val cnxn = new AgentCnxn("laptop", "moreTesting", "server")
//cnxn: com.protegra_ati.agentservices.store.AgentTS.acT.AgentCnxn = AgentCnxn(agent://laptop/invitation#,testing,agent://server/invitation#)

//scala> implicit def toPattern( s : String ) : CnxnCtxtLabel[String,String,String] with Factual = CXQ.fromCaseClassInstanceString( s ).getOrElse( null ).asInstanceOf[CnxnCtxtLabel[String,String,String] with Factual]
implicit def toPattern( s : String ) : CnxnCtxtLabel[String,String,String] with Factual = CXQ.fromCaseClassInstanceString( s ).getOrElse( null ).asInstanceOf[CnxnCtxtLabel[String,String,String] with Factual]
//toPattern: (s: String)com.biosimilarity.lift.model.store.CnxnCtxtLabel[String,String,String] with com.biosimilarity.lift.model.store.Factual

//scala> implicit def toValue( s : String ) : mTT.Resource = mTT.Ground( s )
implicit def toValue( s : String ) : mTT.Resource = mTT.Ground( s )
//toValue: (s: String)com.protegra_ati.agentservices.store.AgentTS.mTT.Resource

//scala> val lbl1 = "contentChannel(\"email\")"
val lbl1 = "contentChannel(\"email\")"
//lbl1: java.lang.String = contentChannel("email")

//scala> val lbl2 = "contactMeUsing(\"email\")"
val lbl2 = "contactMeUsing(\"email\")"
//lbl2: java.lang.String = contactMeUsing("email")

//scala> val value1 = "for the get Jason.klassen@protegra.com"
val value1 = "for the get Jason.klassen@protegra.com"
//value1: java.lang.String = for the get Jason.klassen@protegra.com

//scala> val value2 = "for the win Mikail.Mauz@gmail.com"
val value2 = "for the win Mikail.Mauz@gmail.com"
//value2: java.lang.String = for the win Mikail.Mauz@gmail.com

//scala> cnxn.symmetricIdentityString
cnxn.symmetricIdentityString
//res0: String = testing_3327928445
