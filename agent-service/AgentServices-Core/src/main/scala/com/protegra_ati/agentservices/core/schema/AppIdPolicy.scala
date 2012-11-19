package com.protegra_ati.agentservices.core.schema

/* User: sharder
*/
case object AppIdPolicy extends Enumeration("profileEditable", "businessProfileEditable", "groupProfileEditable")
{
  val ProfileEditable, BusinessProfileEditable, GroupProfileEditable = Value
}