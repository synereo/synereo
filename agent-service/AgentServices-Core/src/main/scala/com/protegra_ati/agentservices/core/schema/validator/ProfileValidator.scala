package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.Profile

/* User: mgevantmakher
*/

trait ProfileValidator extends DataValidator
{
  self: Profile =>

  override def selfValidate: List[ String ] =
  {
    println("ProfileValidator executed")
    var result: List[ String ] = Nil;
    if ( this.firstName == "1" )
      return result
    return result
  }


}
