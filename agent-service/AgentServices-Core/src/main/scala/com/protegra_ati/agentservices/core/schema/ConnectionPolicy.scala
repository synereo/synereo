package com.protegra_ati.agentservices.core.schema

/* User: mgevantmakher
*/
case object ConnectionPolicy extends Enumeration("referralsDisabled", "deleteDisabled", "searchDisabled", "remoteSearchDisabled", "dataSharingEnabled")
{
  val ReferralsDisabled, DeleteDisabled, SearchDisabled, RemoteSearchDisabled, CacheDisabled, DataSharingEnabled = Value
}