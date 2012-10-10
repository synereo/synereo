package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.ContentVerifier

/* User: mgevantmakher
*/

trait ContentVerifierValidator extends DataValidator
{
  self: ContentVerifier =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  { var result: List[ String] = Nil;
    println("ContentVerifierValidator executed")
    if ( this.claimKey == "somekey" )
      return result
    return result
  }


}