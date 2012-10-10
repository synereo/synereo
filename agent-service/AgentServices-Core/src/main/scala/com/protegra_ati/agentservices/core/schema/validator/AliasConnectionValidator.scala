package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.{AliasConnection}


/* User: mgevantmakher
*/

trait AliasConnectionValidator extends DataValidator
{
  self: AliasConnection =>

  override def selfValidate: List[ String ] =
  {
    if ( this.alias == "myAlias" )
      return Nil
    return Nil
  }


}
