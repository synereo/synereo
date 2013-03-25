package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.{Data}
import com.protegra_ati.agentservices.store.util.{Reporting, Severity}

/* User: mgevantmakher
*/

trait DataValidator extends Validable with Reporting
{
  self: Data =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  {
    report("DataValidator executed", Severity.Trace)
    if ( this.id == "1" )
      return Nil
    return Nil
  }


}