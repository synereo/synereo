// -*- mode: Scala;-*- 
// Filename:    ping-pong-setup.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 18 19:23:06 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.usage._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store.xml.datasets._
import com.biosimilarity.lift.lib._
import scala.util.continuations._ 
import java.net.URI
import java.util.UUID
import CCLDSL._
import PersistedMonadicTS._
import com.biosimilarity.lift.lib.SpecialKURIDefaults._

implicit def toPattern(
  s : String
) : CnxnCtxtLabel[String,String,String] with Factual =
  CXQ.fromCaseClassInstanceString(
    s
  ).getOrElse(
    null
  ).asInstanceOf[CnxnCtxtLabel[String,String,String] with Factual]

implicit def toValue( s : String ) : mTT.Resource = mTT.Ground( s )

//val pimgJunq = ptToPt( "K", "10.0.1.5", "10.0.1.9" )
//val pimgJunq = ptToPt( "Yellow", "localhost", "localhost" )
val pimgJunq = ptToPt( "Green", "localhost", "localhost" )
val atps = pimgJunq.agentTwistedPairs

val lbl1 = "contentChannel(\"email\")"
val lbl2 = "contactMeUsing(\"email\")"
val value1 = "for the get Jason.klassen@protegra.com"
val value2 = "for the win Mikail.Mauz@gmail.com"

