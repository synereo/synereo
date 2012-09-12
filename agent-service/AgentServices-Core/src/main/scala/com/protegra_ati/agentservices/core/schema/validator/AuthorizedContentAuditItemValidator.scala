package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.AuthorizedContentAuditItem

/* User: mgevantmakher
*/

trait AuthorizedContentAuditItemValidator extends DataValidator
{
  self: AuthorizedContentAuditItem =>

  override def selfValidate: List[ String ] =
  {
    println("AuthorizedContentAuditItem executed")
    if ( (this.autoApproved==true)  )
      return Nil
    return Nil
  }


}
