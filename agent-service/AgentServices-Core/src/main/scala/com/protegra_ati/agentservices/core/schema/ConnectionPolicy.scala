package com.protegra_ati.agentservices.core.schema

object ConnectionPolicy extends Enumeration {
  type ConnectionPolicy = Value

  val ReferralsDisabled = Value("referralsDisabled")
  val DeleteDisabled = Value("deleteDisabled")
  val SearchDisabled = Value("searchDisabled")
  val RemoteSearchDisabled = Value("remoteSearchDisabled")
  val CacheDisabled = Value("cacheDisabled")
  val BusinessProfileSharingEnabled = Value("businessProfileSharingEnabled")
  val GroupPostSharingEnabled = Value("groupPostSharingEnabled")
  val RoleSharingEnabled = Value("roleSharingEnabled")
}