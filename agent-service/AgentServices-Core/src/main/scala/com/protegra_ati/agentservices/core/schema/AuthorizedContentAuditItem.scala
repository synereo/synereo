package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.schema.validator.AuthorizedContentAuditItemValidator
import com.protegra_ati.agentservices.core.schema.persistence._
/* User: mtodd
*/
import scala.reflect.BeanProperty

/*
autoApproved accepts "true", "false", or "" as a default value
 */
case class AuthorizedContentAuditItem(@BeanProperty var objectType:String, @BeanProperty var connectionType:String, @BeanProperty var fields:String, @BeanProperty var autoApproved: String) extends Data
with AuthorizedContentAuditItemValidator
with StorableAuthorizedContentAuditItemDataDefaults{
 // TODO implement the toSearchKey method eventually!!!
 def this () = this ("", "", "", "")
}