import scala.util.continuations._
import java.net.URI

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._

import com.protegra_ati.agentservices.store.mongo.usage._

val sourceAddress = "127.0.0.1".toURI
val acquaintanceAddresses = List[ URI ]()
val configFileName = Some("db_store.conf")
val space = AgentUseCase(configFileName)
val node = space.createNode(sourceAddress, acquaintanceAddresses, configFileName)

val cnxn = new AgentCnxn("pubsubExact".toURI, "", "Jason".toURI)

val lbl = ( "contentChannel(\"123\")" ).toLabel
val value = "testtest"

val lblSearch = "contentChannel(_)".toLabel
reset { for( e <- node.subscribe(cnxn)( lbl ) ) { println( "received: " + e) } }


reset { node.publish(cnxn)(lbl, Ground(value)) }

