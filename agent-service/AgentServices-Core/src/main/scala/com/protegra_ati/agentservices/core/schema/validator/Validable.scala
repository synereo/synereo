package com.protegra_ati.agentservices.core.schema.validator

/* User: mgevantmakher
*/

trait Validable
{
  protected def parentValidate: List[ String ] =
  {Nil}

  protected def selfValidate: List[ String ] =
  {Nil}

  // this can be terrible inefficient, don't call them together with getValidationResults
  def isValid(): Boolean =
  {
    ( getValidationResults.count _ == 0 )
  }

  def getValidationResults: List[ String ] =
  {return parentValidate ::: selfValidate }
}