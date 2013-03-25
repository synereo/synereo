package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

/* User: mgevantmakher
*/

trait ProfileValidator extends DataValidator with Reporting
{
  self: Profile =>

  override def selfValidate: List[ String ] =
  {
    report("ProfileValidator executed", Severity.Trace)
    var result: List[ String ] = Nil;
    if ( this.firstName == "1" )
      return result
    return result
  }


}
