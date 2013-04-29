package com.protegra_ati.agentservices.core.schema

//lower case values necessary for prolog label eval (camelcase)
object AppIdPolicy extends Enumeration {
  type AppIdPolicy = Value

  val ProfileEditable = Value("profileEditable")
  val BusinessProfileEditable = Value("businessProfileEditable")
  val GroupProfileEditable = Value("groupProfileEditable")
}