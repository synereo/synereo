/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages

//string list for Enum constructor is required for serialization
//val must be kept in same order as enum constructor
//lower case necessary for prolog label eval (camelcase)
object Channel extends Enumeration("global", "admin", "registration", "security", "content", "invitation", "introduction", "referral", "verify", "survey", "search")
{
  type Channel = Value
  val Global, Admin, Registration, Security, Content, Invitation, Introduction, Referral, Verify, Survey, Search, Notification = Value
}
