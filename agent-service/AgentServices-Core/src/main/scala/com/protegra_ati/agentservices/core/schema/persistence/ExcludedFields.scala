package com.protegra_ati.agentservices.core.schema.persistence

/* User: mgevantmakher
*/

/**
 * Fields excluded by persistence (toSearchKey and toStoreKey)
 */
object ExcludedFields
{
  val excludedFields = List[ String ]("versionNumber")//, "isKryoSerializationDeprecated", "isJavaIOSerializationDeprecated")

}
