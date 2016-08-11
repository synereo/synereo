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

val cnxn = new AgentCnxn("Del2Test".toURI, "", "Jason".toURI)

val lbl = ( "contentChannel(\"123\")" ).toLabel
val value = "testtest"

node.store(cnxn)(lbl, Ground(value))

val lblSearch = "contentChannel(_)".toLabel

reset { for( e <- node.read(cnxn)( lblSearch ) ) { println( "received: " + e) } }




reset { for( e <- node.read(true)(cnxn)( lblSearch ) ) { println( "received: " + e) } }


val lblTest = "data(connection(keys(id(), localeCode(_), recVerNum(_)), _))".toLabel
val cnxnRead = new AgentCnxn("f1a48b74-6a4a-455a-858d-a6b7798cb71c".toURI, "", "f1a48b74-6a4a-455a-858d-a6b7798cb71c".toURI)

reset { for( e <- node.read(cnxnRead)( lblTest ) ) { println( "received: " + e) } }

val lblWorkTest = "data(connection(keys(id(_), localeCode(_), recVerNum(_)), _))".toLabel
reset { for( e <- node.read(cnxnRead)( lblWorkTest ) ) { println( "received: " + e) } }
