package com.protegra_ati.agentservices.core.schema

import tools.nsc.doc.model.Public
import reflect.BeanProperty
import reflect.BeanInfo

/* User: mtodd
*/

case class CompositeData[ T <: Data ](@BeanProperty val connection: Connection, @BeanProperty val data: T) extends Data
{

  def this() = this(null, null.asInstanceOf[T])

 //???? @BeanProperty val compositeId = data.hashCode().toString + connection.id.toString
  //at this point, we do not want to be able to save this data
  override def toStoreKey: String =
  {
    throw new Exception("Can't save a composite object")
  }
 }