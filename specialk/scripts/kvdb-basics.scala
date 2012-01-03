// -*- mode: Scala;-*- 
// Filename:    kvdb-basics.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan  2 14:01:03 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.usage._
import com.biosimilarity.lift.model.store.usage.PersistedMonadicTS._
import scala.util.continuations._ 
import CnxnConversionStringScope._

object Acceptance {
  case class CC1( b : Boolean, i : Int, s : String, r : Option[CC1] )
  case class CC2( b : Boolean, i : Int, s : String )

  lazy val kvdb1 = singleton( "Acceptance", "localhost" )
  lazy val cc11 = CC1( true, 0, "Fire", None )
  lazy val cc21 = CC2( true, 0, "Ice" )
  lazy val xelem1 = <CC1><b>true</b><i>0</i><s>"Fire"</s><r>None</r></CC1>
  
  lazy val v =
    reset { for( e <- kvdb1.get( CC1( true, 0, "Fire", None ) ) ) { println( "received: " + e ); e } }

  lazy val s = {
    cclStr = asCnxnCtxtLabel( "Steam" )
    reset { kvdb1.put( CC1( true, 0, "Fire", None ), cclStr ) };
    cclStr
  }
}
