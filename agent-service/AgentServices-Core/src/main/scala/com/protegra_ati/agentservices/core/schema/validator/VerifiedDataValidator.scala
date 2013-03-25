package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

/* User: mgevantmakher
*/

trait VerifiedDataValidator extends DataValidator with Reporting
{
  self: VerifiedData =>

  override def selfValidate: List[ String ] =
  {
    report("ProfileValidator executed", Severity.Trace)
    var result: List[ String ] = Nil;
    if ( this.alias == "Myalias" )
      return result
    return result
  }


}
