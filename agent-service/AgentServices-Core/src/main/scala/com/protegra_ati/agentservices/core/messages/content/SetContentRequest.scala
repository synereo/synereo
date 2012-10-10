/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty

case class SetContentRequest(override val eventKey: EventKey, newData: Data, oldData: Data) extends Message(new Identification(), eventKey)  //extends Message(eventKey) kryo workaround
with Request
{  def this()= this (null,null,null) // serialization helper
  override def channel = Channel.Content


}
