

import scala.util.continuations._
import java.net.URI
import java.util.UUID
import com.protegra_ati.agentservices.store._
import com.protegra_ati.agentservices.store.AgentTS._
import com.protegra_ati.agentservices.store.AgentTS.acT._
import com.protegra_ati.agentservices.store.AgentTS.mTT._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

val sourceAddress = "172.23.32.158"
val acquaintanceAddresses =  "172.23.32.62"
val pimgJunq =  ptToPt(sourceAddress, acquaintanceAddresses)
pimgJunq.database
pimgJunq.agentTwistedPairs
val lbl = "contentChannel(\"email\")".toLabel
val cnxn = new AgentCnxn("Mike".toURI, "", "Jason".toURI)
reset { for( e <- pimgJunq.get(cnxn)( lbl ) ) { println( "received: " + e.dispatch ) } }


val sourceAddress = "172.23.32.253".toURI
val acquaintanceAddresses =  List[URI]("172.23.32.62".toURI)
val pimgJunq =  new AgentStringPersistence.PartitionedStringMGJ(sourceAddress, acquaintanceAddresses, None)