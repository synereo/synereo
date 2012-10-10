package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema._

/* User: mgevantmakher
*/

trait VerifiedDataValidator extends DataValidator
{
  self: VerifiedData =>

  override def selfValidate: List[ String ] =
  {
    println("ProfileValidator executed")
    var result: List[ String ] = Nil;
    if ( this.alias == "Myalias" )
      return result
    return result
  }


}
