package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.{Connection}

/* User: mgevantmakher
*/

trait ConnectionValidator extends DataValidator
{
  self: Connection =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  {
    println("ConnectionValidator executed")
    if ( this.connectionType == "friend" )
      return Nil
    return Nil
  }


}