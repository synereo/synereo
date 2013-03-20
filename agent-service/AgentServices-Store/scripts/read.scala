
import scala.util.continuations._
import java.net.URI
import java.util.UUID
import com.protegra_ati.agentservices.store._

import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
//import com.protegra_ati.agentservices.store.usage.AgentUseCase._
//import Being.AgentKVDBNodeFactory

import com.protegra_ati.agentservices.store.mongo.usage._

val sourceAddress = "127.0.0.1".toURI
val acquaintanceAddresses = List[ URI ]()
val configFileName = Some("db_store.conf")
//val node = AgentKVDBNodeFactory.ptToMany(sourceAddress, acquaintanceAddresses)(configFileName)
val space = AgentUseCase(configFileName)
val node = space.createNode(sourceAddress, acquaintanceAddresses, configFileName)

val cnxn = new AgentCnxn("Test1".toURI, "", "Jason".toURI)

val lbl = ( "contentChannel(\"123\")" ).toLabel
val value = "testtest"
reset { node.publish(cnxn)(lbl, Ground(value)) }

//node.store(cnxn)(lbl, Ground(value))

val lblSearch = "contentChannel(_)".toLabel

reset { for( e <- node.subscribe(cnxn)( lblSearch ) ) { println( "received: " + e) } }
reset { for( e <- node.fetch(cnxn)( lblSearch ) ) { println( "received: " + e.dispatch ) } }



