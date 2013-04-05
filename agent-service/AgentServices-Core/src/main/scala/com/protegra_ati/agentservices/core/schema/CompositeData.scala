package com.protegra_ati.agentservices.core.schema

import reflect.BeanProperty
import java.util.ArrayList
import Constants._

/* User: mtodd
*/

case class CompositeData[ T <: Data ](@BeanProperty val connections: ArrayList[Connection], @BeanProperty val data: T) extends Data
{
  def this(connection: Connection, data: T) = {
    this(new ArrayList[Connection](), data)
    connections.add(connection)
  }
  def this() = this(new ArrayList[Connection](), null.asInstanceOf[T])

  override def toStoreKey: String =
  {
    CompositeData.STOREKEY_PREFIX + data.toStoreKey + CompositeData.STOREKEY_POSTFIX
  }

  override def toSearchKey: String =
  {
    CompositeData.STOREKEY_PREFIX + data.toSearchKey + CompositeData.STOREKEY_POSTFIX
  }

 }

object CompositeData {
  final val STOREKEY_PREFIX = "compositeData(" + FIELDS + "("
  final val STOREKEY_POSTFIX = "))"
}