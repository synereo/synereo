package com.protegra_ati.agentservices.core.messages

//lower case values necessary for prolog label eval (camelcase)
object Channel extends Enumeration {
  type Channel = Value

  val Global = Value("global")
  val Admin = Value("admin")
  val Registration = Value("registration")
  val Security = Value("security")
  val Content = Value("content")
  val Invitation = Value("invitation")
  val Introduction = Value("introduction")
  val Referral = Value("referral")
  val Verify = Value("verify")
  val Survey = Value("survey")
  val Search = Value("search")
  val Notification = Value("notification")
}
