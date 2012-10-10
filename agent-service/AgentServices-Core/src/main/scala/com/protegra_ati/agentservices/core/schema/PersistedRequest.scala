package com.protegra_ati.agentservices.core.schema

/* User: mtodd
*/
import com.protegra_ati.agentservices.core.messages._
import scala.reflect.BeanProperty
import org.joda.time.DateTime

//deprecated?
case class PersistedRequest(@BeanProperty messageType:String, @BeanProperty message:Message, @BeanProperty dateLogged:DateTime) extends Data(){
  def this() = this (null, null, null)
}