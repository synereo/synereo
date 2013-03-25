package com.protegra_ati.agentservices.core.schema.validator


import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

/* User: mgevantmakher
*/

trait AuthorizationRequestValidator extends DataValidator with Reporting
{
  self: AuthorizationRequest =>

  override def selfValidate: List[ String ] =
  {
    report("AuthorizationRequestValidator executed", Severity.Trace)
    if ( this.objectName == "objectName" )
      return Nil
    return Nil
  }


}
