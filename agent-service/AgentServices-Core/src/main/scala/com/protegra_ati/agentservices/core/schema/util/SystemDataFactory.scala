package com.protegra_ati.agentservices.core.schema.util


import com.protegra_ati.agentservices.core.schema._


/* User: mgevantmakher
*/

object SystemDataFactory
{

  final private val SEARCH_ALL_SYSTEM_DATA_OF_CONNECTION_KEY = ( new SystemData[ Connection ](ConnectionFactory.createEmptyImmutableConnectionForSearch()) ).toSearchKey

  final private val SEARCH_ALL_SYSTEM_DATA_OF_CONNECTION = new SystemData[ Connection ](ConnectionFactory.createEmptyImmutableConnectionForSearch())
  {
    // prevents several 100T of strings and reflection calls
    override def toSearchKey(): String = SystemDataFactory.SEARCH_ALL_SYSTEM_DATA_OF_CONNECTION_KEY
  }

  // use everywhere where before new SystemData[ Connection ] (new Connection()) was used without any modifications!!!!
  // NEVER modify connection object or SystemData returned over this method!!!!!!
  def createEmptyImmutableSystemDataForConnectionSearch(): SystemData[ Connection ] =
  {
    SEARCH_ALL_SYSTEM_DATA_OF_CONNECTION
  }
}
