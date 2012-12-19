package com.protegra_ati.agentservices.core.schema

import tools.nsc.doc.model.Public
import reflect.BeanProperty
import reflect.BeanInfo
import Constants._
/* User: mtodd
*/

case class CompositeData[ T <: Data ](@BeanProperty val connection: Connection, @BeanProperty val data: T) extends Data
{
  def this() = this(null, null.asInstanceOf[T])

  override def toStoreKey: String =
  {
    CompositeData.STOREKEY_PREFIX + connection.toStoreKey + CompositeData.STOREKEY_DELIMITER + data.toStoreKey + CompositeData.STOREKEY_POSTFIX
  }

  override def toSearchKey: String =
  {
    CompositeData.STOREKEY_PREFIX + connection.toSearchKey + CompositeData.STOREKEY_DELIMITER + data.toSearchKey + CompositeData.STOREKEY_POSTFIX
  }

 }

object CompositeData {
  final val STOREKEY_PREFIX = "compositeData(" + FIELDS + "("
  final val STOREKEY_DELIMITER = ","
  final val STOREKEY_POSTFIX = "))"
}