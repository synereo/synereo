package com.protegra_ati.agentservices.core.schema

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.reflect.BeanProperty
import util._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import scala.annotation.unchecked.uncheckedVariance
import com.protegra_ati.agentservices.core.extensions.ClassExtensions._

case class AllDisclosedData() extends Data

object AllDisclosedData {
  def INSTANCE = (new AllDisclosedData).getClassOf
}

//important to note that dataDisplayClassName matches up with Data.toStoreKey
case class DisclosedData[ +T <: Data ](@BeanProperty var dataClassType: Class[ T @uncheckedVariance], @BeanProperty var connectionType: String, @BeanProperty var fields: String) extends Data
with ExcludeFromReplication
{
  def this(@BeanProperty connectionType:String) = this( AllDisclosedData.INSTANCE.asInstanceOf[Class[ T @uncheckedVariance]], connectionType, "" )
  def this(@BeanProperty dataClassType: Class[ T @uncheckedVariance]) = this (dataClassType, "", "")
  def this () = this (AllDisclosedData.INSTANCE.asInstanceOf[Class[ T @uncheckedVariance]], "", "")

  //important to note that dataDisplayClassName matches up to Data.formattedClassName
  def dataDisplayClassName():String = {
   // if (dataClassType==null)  println ("-------------------------------------------NUL NUL NUL NUL NUL-------------------------------------") for nothing good?
    dataClassType.getName.trimPackage.toCamelCase
  }

  def forAudit(connection: Connection): AuthorizedContentAuditItem =
  {
    new AuthorizedContentAuditItem(this.dataDisplayClassName, this.connectionType, this.fields, connection.autoApprove)
  }

  def toSearchKeyFromDataClassType(): String =
  {
    toSearchKeyFromClass(dataClassType)
  }

  // builds the search key, for this object to be used for search like a search command repository
  override def toSearchKey: String =
  {
    if (classOf[AllDisclosedData].isAssignableFrom(dataClassType)) {
      // We're using AllDisclosedData as SEARCH_ALL.  toSearchKey logic will not handle that properly
      // unless it is passed null as dataClass type, so we temporarly flip it to null
      // For safety purposes, we flip back to old dataClassType when done, in case anything ever needs it again
      val oldDataClassType = dataClassType
      dataClassType = null
      val searchKey = super.toSearchKey
      dataClassType = oldDataClassType
      searchKey
    } else {
      super.toSearchKey
    }
  }
}


object DisclosedData
{
  final val SEARCH_ALL_KEY = new DisclosedData().toSearchKey

  final val SEARCH_ALL = new DisclosedData()
  {
    override def toSearchKey(): String = DisclosedData.SEARCH_ALL_KEY
  }

}
