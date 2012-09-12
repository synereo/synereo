package com.protegra_ati.agentservices.core.schema

/* User: mtodd
*/
import scala.reflect.BeanProperty
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import com.protegra_ati.agentservices.core.schema.validator._

case class AuthorizationRequest(@BeanProperty objectName:String, @BeanProperty msg:GetContentRequest, @BeanProperty dateLogged:DateTime, @BeanProperty var approved:String) extends Data
with AuthorizationRequestValidator{
   def this () = this("", null, null, "false")
}