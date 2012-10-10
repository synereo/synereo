package com.protegra_ati.agentservices.core.schema.validator

import com.protegra_ati.agentservices.core.schema.{Data}

/* User: mgevantmakher
*/

trait DataValidator extends Validable
{
  self: Data =>

  override protected def parentValidate: List[ String ] =
  {
    getValidationResults
  }

  override def selfValidate: List[ String ] =
  {   println("DataValidator executed")
    if ( this.id == "1" )
      return Nil
    return Nil
  }


}