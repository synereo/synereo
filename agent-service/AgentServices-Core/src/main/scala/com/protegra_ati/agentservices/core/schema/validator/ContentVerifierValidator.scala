package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.ContentVerifier
import com.protegra_ati.agentservices.store.util.{Reporting, Severity}

/* User: mgevantmakher
*/

trait ContentVerifierValidator extends DataValidator with Reporting
{
  self: ContentVerifier =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  {
    var result: List[ String] = Nil
    report("ContentVerifierValidator executed", Severity.Trace)
    if ( this.claimKey == "somekey" )
      return result
    return result
  }


}