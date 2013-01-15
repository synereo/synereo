package com.protegra_ati.agentservices.core.schema.util


import com.protegra_ati.agentservices.core.schema._


/* User: mgevantmakher
*/

object SystemDataFactory
{

  final private val SEARCH_ALL_CONNECTION_KEY = ( new SystemData[ Connection ](Connection.SEARCH_ALL) ).toSearchKey

  final val SEARCH_ALL_CONNECTION = new SystemData[ Connection ](Connection.SEARCH_ALL)
  {
    // prevents several 100T of strings and reflection calls
    override def toSearchKey(): String = SystemDataFactory.SEARCH_ALL_CONNECTION_KEY
  }
}
