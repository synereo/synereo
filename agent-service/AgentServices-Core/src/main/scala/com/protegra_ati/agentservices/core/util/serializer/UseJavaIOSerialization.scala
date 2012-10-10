package com.protegra_ati.agentservices.core.util.serializer

/* User: mgevantmakher
*/

/**
 * Marker trait/interface
 */
trait UseJavaIOSerialization extends VersionNumber
{
  def isJavaIOSerializationDeprecated: Boolean = false
}
