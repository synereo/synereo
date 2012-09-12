package com.protegra_ati.agentservices.core.util.serializer

/* User: mgevantmakher
*/

/**
 * Marker trait/interface
 */
trait UseKryoSerialization extends Serializable with VersionNumber
{
  def isKryoSerializationDeprecated: Boolean = false
}
