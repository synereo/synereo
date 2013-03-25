package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.AuthorizedContentAuditItem
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

/* User: mgevantmakher
*/

trait AuthorizedContentAuditItemValidator extends DataValidator with Reporting
{
  self: AuthorizedContentAuditItem =>

  override def selfValidate: List[ String ] =
  {
    report("AuthorizedContentAuditItem executed", Severity.Trace)
    if ( (this.autoApproved==true)  )
      return Nil
    return Nil
  }


}
