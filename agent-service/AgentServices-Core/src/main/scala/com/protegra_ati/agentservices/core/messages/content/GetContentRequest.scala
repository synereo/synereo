/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty

case class GetContentRequest(override val eventKey: EventKey, queryObject: Data) extends Message(new Identification(), eventKey)//extends Message(eventKey)-- kryo workaround
  with Request
  with NotificationProducer
{
  def this() = this(null, null)

//  def this(msg:GetContentRequest) = this(msg.eventKey, msg.queryObject)
  override def channel = Channel.Content

  @BeanProperty var contentConnection: Option[ Connection ] = None

  override def generateNotification(key:EventKey): Message with Notification = {
    new GetContentAuthorizationRequiredNotification(key)
  }

}
