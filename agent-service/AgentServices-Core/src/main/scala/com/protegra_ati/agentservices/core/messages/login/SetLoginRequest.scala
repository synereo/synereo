package com.protegra_ati.agentservices.core.messages.login

/* User: mtodd
*/
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import java.util._

case class SetLoginRequest( override val eventKey: EventKey, loginToken:LoginToken) extends Message(new Identification(), eventKey)//extends Message(eventKey)
with Request {
  def this() = this(null, null)

  override def channel = Channel.Security
}