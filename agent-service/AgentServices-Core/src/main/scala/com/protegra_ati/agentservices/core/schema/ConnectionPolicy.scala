package com.protegra_ati.agentservices.core.schema

/* User: mgevantmakher
*/
case object ConnectionPolicy extends Enumeration("referralsDisabled", "deleteDisabled", "searchDisabled", "remoteSearchDisabled", "businessProfileSharingEnabled", "groupPostSharingEnabled", "roleSharingEnabled")
{
  val ReferralsDisabled, DeleteDisabled, SearchDisabled, RemoteSearchDisabled, CacheDisabled, BusinessProfileSharingEnabled, GroupPostSharingEnabled, RoleSharingEnabled = Value
}