package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.extensions.ClassExtensions._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.messages.content._


trait Authorization
{
  self: AgentHostStorePlatformAgent
    with Storage =>

  def authorizationNotRequired(request: GetContentRequest): Boolean =
  {
    var bypassAuthorization: Boolean = false
    request.queryObject match {
      case x: Introduction => {
        bypassAuthorization = true
      }
      case x: Post => {
        bypassAuthorization = true
      }
      case _ => {
        //disable for now. whne we reenable we may have to revamp AuthorizedContentAuditItem to be related to DisclosedData
        //approve or deny based on class and trust level not per field?
        bypassAuthorization = true
      }
    }
    bypassAuthorization
  }

  /**
   * Changes disclosed data (from old to new) for a newConnection stored in a selfConnection
   * @param newConnection
   * @param newDisclosedData
   * @param oldDisclosedData
   * @param selfCnxn
   */
  def changeDisclosedContentOnConnection(selfCnxn: AgentCnxnProxy, newConnection: Connection, newDisclosedData: DisclosedData[ Data ], oldDisclosedData: DisclosedData[ Data ]) =
  {
    findAllDataAndChangeDisclosure(selfCnxn, newDisclosedData, oldDisclosedData, newConnection)
  }

  def changeDisclosedContentOnConnection(newConnection: Connection, oldConnection: Connection, selfCnxn: AgentCnxnProxy) =
  {
    if ( oldConnection != null ) {
      //search new auth content by conn type
      val disclosedDataExistingSearch = new DisclosedData(oldConnection.connectionType)
      fetchList[ DisclosedData[ Data ] ](_dbQ, selfCnxn, disclosedDataExistingSearch.toSearchKey, findNewDisclosure(_: AgentCnxnProxy, _: List[ DisclosedData[ Data ] ], newConnection))
    }
    else {
      findNewDisclosure(selfCnxn, List[ DisclosedData[ Data ] ](), newConnection)
    }
  }

  def findNewDisclosure(selfCnxn: AgentCnxnProxy, oldDisclosedData: List[ DisclosedData[ Data ] ], newConnection: Connection)
  {
    report("entering findAllDataAndChangeDisclosure in StorePlatform", Severity.Trace)

    val disclosedDataToSetSearch = new DisclosedData(newConnection.connectionType)
    fetchList[ DisclosedData[ Data ] ](_dbQ, selfCnxn, disclosedDataToSetSearch.toSearchKey, findAllDataAndChangeDisclosure(_: AgentCnxnProxy, _: List[ DisclosedData[ Data ] ], oldDisclosedData, newConnection))

    report("exiting findAllDataAndChangeDisclosure", Severity.Trace)
  }


  def findAllDataAndChangeDisclosure(selfCnxn: AgentCnxnProxy, newDisclosedData: DisclosedData[ Data ], oldDisclosedData: DisclosedData[ Data ], newConnection: Connection)
  {

   // println("!!!!findAllDataAndChangeDisclosure new disclose data: " + newDisclosedData)

    report("entering findAllDataAndChangeDisclosure in StorePlatform", Severity.Trace)

    if ( !newDisclosedData.dataDisplayClassName.equals(oldDisclosedData.dataDisplayClassName()) )
      return

    compareDisclosure(
      selfCnxn,
      Some(newDisclosedData),
      Some(oldDisclosedData),
      newConnection)
    report("exiting findAllDataAndChangeDisclosure", Severity.Trace)
  }

  def findAllDataAndChangeDisclosure(selfCnxn: AgentCnxnProxy, newDisclosedData: List[ DisclosedData[ Data ] ], oldDisclosedData: List[ DisclosedData[ Data ] ], newConnection: Connection)
  {
    //println("!!!!findAllDataAndChangeDisclosure new disclose data: " + newDisclosedData)
    report("entering findAllDataAndChangeDisclosure in StorePlatform", Severity.Trace)
    val classNames = newDisclosedData.map(x => x.dataDisplayClassName) ::: oldDisclosedData.map(x => x.dataDisplayClassName)
    val uniqueClassNames = classNames.distinct

    uniqueClassNames.map(className => compareDisclosure(
      selfCnxn,
      findDisclosedDataByClassName(className, newDisclosedData),
      findDisclosedDataByClassName(className, oldDisclosedData),
      newConnection)
    )

    report("exiting findAllDataAndChangeDisclosure", Severity.Trace)
  }

  def findDisclosedDataByClassName(className: String, disclosedData: List[ DisclosedData[ Data ] ]): Option[ DisclosedData[ Data ] ] =
  {
    val data = disclosedData.filter(x => x.dataDisplayClassName == className)
    if ( data.length > 1 )
      report("too many DisclosedData objects, choosing the head", Severity.Error)

    data.headOption
  }

  def compareDisclosure(selfCnxn: AgentCnxnProxy, newDisclosedData: Option[ DisclosedData[ Data ] ], oldDisclosedData: Option[ DisclosedData[ Data ] ], newConnection: Connection) =
  {
    //println("COMPARE DISCLOSURE!!!!!!!!")
    (newDisclosedData, oldDisclosedData) match {
      //no disclosed data found should delete existing data
      case (None, _) => {
        //        val dataToChangeSearchKey = SearchFactory.toSearchKeyFromClassName(className)
        //        fetch[ Data ](_dbQ, selfCnxn, dataToChangeSearchKey, delete(_: AgentCnxnProxy, _: Data))
        //delete
      }
      //TODO: more efficient to combine in one case
      case (Some(newDisclosedDatum), None) => {
        val dataToChangeSearchKey = newDisclosedDatum.toSearchKeyFromDataClassType()
        fetch[ Data ](_dbQ, selfCnxn, dataToChangeSearchKey, findOldDataAndChangeDisclosure(_: AgentCnxnProxy, _: Data, newDisclosedDatum, null, newConnection))
      }
      case (Some(newDisclosedDatum), Some(oldDisclosedDatum)) if newDisclosedDatum != oldDisclosedDatum => {
        //println("PROCESS DISCLOSURE!!!!!!!!")

        //TODO: possibly optimize of oldData not found but let the insertUpdate logic handle for now
        val dataToChangeSearchKey = newDisclosedDatum.toSearchKeyFromDataClassType()
        fetch[ Data ](_dbQ, selfCnxn, dataToChangeSearchKey, findOldDataAndChangeDisclosure(_: AgentCnxnProxy, _: Data, newDisclosedDatum, oldDisclosedDatum, newConnection))
      }
    }
  }

  def findOldDataAndChangeDisclosure(cnxn: AgentCnxnProxy, selfData: Data, newDisclosedData: DisclosedData[ Data ], oldDisclosedData: DisclosedData[ Data ], newConnection: Connection)
  {
    report("entering findOldDataAndChangeDisclosure in StorePlatform", Severity.Trace)

    val newAuthorizedData = selfData.authorizedData(newDisclosedData.fields)
    var oldAuthorizedData: Data = null
    if ( oldDisclosedData != null )
      oldAuthorizedData = selfData.authorizedData(oldDisclosedData.fields)
    // eventually  parentRequestIds has to be forwarded
    handleSetContentByConnectionTypeFetch(cnxn, newConnection, newAuthorizedData, oldAuthorizedData, newDisclosedData)

    report("exiting findOldDataAndChangeDisclosure", Severity.Trace)
  }


  //  def retrieveNewAuthorizationInfo(cnxn: AgentCnxnProxy, oldAuthorization: DisclosedData[ Data ], oldConnection: Connection, newConnection: Connection, data: Data)
  //  {
  //    val contentSearch = new DisclosedData(data.getClassOf, newConnection.connectionType, "")
  //
  //    fetch[ DisclosedData[ Data ] ](_dbQ, cnxn, contentSearch.toSearchKey, handlePermissionsOnViewableItemByCnxnFetch(_: AgentCnxnProxy, _: DisclosedData[ Data ], oldAuthorization, data, newConnection, oldConnection))
  //  }
  //
  //  def handlePermissionsOnViewableItemByCnxnFetch(cnxn: AgentCnxnProxy, newAuthorizationPermissions: DisclosedData[ Data ], oldAuthorizationPermissions: DisclosedData[ Data ], viewableItem: Data, newConnection: Connection, oldConnection: Connection)
  //  {
  //    val newFieldList = newAuthorizationPermissions.fields.split(',').toList
  //    val newAuthorizedData = viewableItem.authorizedData(newFieldList)
  //    var oldAuthorizedData: Data = null
  //    var oldAuditItem: AuthorizedContentAuditItem = null
  //
  //    if ( oldAuthorizationPermissions != null ) {
  //      val oldFieldList = oldAuthorizationPermissions.fields.split(',').toList
  //      oldAuthorizedData = viewableItem.authorizedData(oldFieldList)
  //      oldAuditItem = oldAuthorizationPermissions.forAudit(oldConnection)
  //    }
  //    updateDataById(newConnection.writeCnxn, newAuthorizedData, oldAuthorizedData)
  //    //we now need to store an authorizedContentAuditItem object in each Connection junction
  //    //for audit purposes
  //
  //    updateDataById(newConnection.writeCnxn, newAuthorizationPermissions.forAudit(newConnection), oldAuditItem)
  //  }
  //
  //  def retrievePreviousAuthorizationInfo(cnxn: AgentCnxnProxy, data: Data, newConnection: Connection, oldConnection: Connection)
  //  {
  //    val contentSearch = new DisclosedData(data.getClassOf, oldConnection.connectionType, "")
  //
  //    val oldAuthorizedContentKey = contentSearch.toSearchKey
  //
  //    fetch[ DisclosedData[ Data ] ](_dbQ, cnxn, contentSearch.toSearchKey, retrieveNewAuthorizationInfo(_: AgentCnxnProxy, _: DisclosedData[ Data ], oldConnection, newConnection, data))
  //
  //  }
}