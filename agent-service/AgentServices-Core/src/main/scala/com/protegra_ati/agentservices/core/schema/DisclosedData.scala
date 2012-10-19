package com.protegra_ati.agentservices.core.schema

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.reflect.BeanProperty
import util._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import scala.annotation.unchecked.uncheckedVariance


//important to note that dataDisplayClassName matches up with Data.toStoreKey
case class DisclosedData[ +T <: Data ](@BeanProperty dataClassType: Class[ T @uncheckedVariance], @BeanProperty var connectionType: String, @BeanProperty var fields: String) extends Data
with ExcludeFromReplication
{
  def this(@BeanProperty connectionType:String) = this (null, connectionType, "")
  def this(@BeanProperty dataClassType: Class[ T @uncheckedVariance]) = this (dataClassType, "", "")
  def this () = this (null, "", "")

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

}
