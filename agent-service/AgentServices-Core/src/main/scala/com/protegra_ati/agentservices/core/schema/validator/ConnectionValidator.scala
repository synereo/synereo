package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.{Connection}
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

/* User: mgevantmakher
*/

trait ConnectionValidator extends DataValidator with Reporting
{
  self: Connection =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  {
    report("ConnectionValidator executed", Severity.Trace)
    if ( this.connectionType == "friend" )
      return Nil
    return Nil
  }


}