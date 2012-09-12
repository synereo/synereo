package com.protegra_ati.agentservices.core.schema.disclosure

/* User: mgevantmakher
*/

/**
 * Constants of enum style for default disclosed data trust levels
 */
case object TrustLevel extends Enumeration("Full", "Introduced", "Basic", "Empty")
{
  val Full, Introduced, Basic, Empty = Value
}
