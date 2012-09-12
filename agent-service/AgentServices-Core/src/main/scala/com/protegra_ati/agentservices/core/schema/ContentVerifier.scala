package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.schema.validator.ContentVerifierValidator
import reflect.BeanProperty

/* User: 
*/

case class ContentVerifier(@BeanProperty var connectionId:String, @BeanProperty var alias:String, @BeanProperty var claimKey:String, @BeanProperty var claimingAgentAlias:String, @BeanProperty var verifierPublicId:String, @BeanProperty var autoApprove:String, @BeanProperty var status:String) extends Data 
with ExcludeFromAudit
with ContentVerifierValidator{
  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  //see if false is expected as default
  def this() = this("", "", "", "", "", "false", "")
}