package com.protegra_ati.agentservices.core.schema.validator

trait Validable {
  protected def parentValidate: List[String] = Nil
  protected def selfValidate: List[String] = Nil

  // this can be terrible inefficient, don't call them together with getValidationResults
  def isValid: Boolean = getValidationResults.isEmpty
  def getValidationResults: List[String] = parentValidate ::: selfValidate
}
