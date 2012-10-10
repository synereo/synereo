package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema._

/* User: mgevantmakher
*/

trait StorableAuthorizedContentAuditItemDataDefaults extends
StorableDataDefaults
{
  self: AuthorizedContentAuditItem =>
  def DEFAULT_AUTO_APPROVED_VALUE = "false"

  // Sets default values to be stored in the storage. Has to be overridden in each Data subclass where default values are nulls or string coded booleans etc.
  // Be careful, this breaks immutability, after this method is called, the object on which this method was called cant be reused for something else as to store data
  // TODO may be it worse it to create a copy/deep clone of the object in a toSearchKey on the fly for one way use to build key to prevent a immutability break
  override def setDefaultValues(isChild:Boolean) =
  {
    super.setDefaultValues(isChild)
    if ( autoApproved == null || autoApproved.equals("") )
      autoApproved = DEFAULT_AUTO_APPROVED_VALUE
  }

}