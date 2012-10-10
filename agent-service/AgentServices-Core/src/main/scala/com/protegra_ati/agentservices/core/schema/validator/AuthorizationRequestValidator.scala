package com.protegra_ati.agentservices.core.schema.validator


import com.protegra_ati.agentservices.core.schema._

/* User: mgevantmakher
*/

trait AuthorizationRequestValidator extends DataValidator
{
  self: AuthorizationRequest =>

  override def selfValidate: List[ String ] =
  {
    println("AuthorizationRequestValidator executed")
    if ( this.objectName == "objectName" )
      return Nil
    return Nil
  }


}
