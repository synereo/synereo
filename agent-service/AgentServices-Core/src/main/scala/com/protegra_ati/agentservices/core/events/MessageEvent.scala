/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.events

import java.util.EventObject
import com.protegra_ati.agentservices.core.messages._
import content._
import reflect.BeanProperty

abstract class MessageEvent[T <: Message](source:T, _msg:T) extends EventObject(source){

  def this(source:T) = this (source, source)

  //BeanProperty needs these
  var msg:T = _msg
  def setMsg(source: T) { this.msg = source }
  def getMsg: T = this.msg
  //End BeanProperty

  def trigger(listeners:List[ _ <: MessageEventAdapter ]) = {
     listeners.map {x => synchronized { triggerEvent(x) } }
  }

  def triggerEvent(adapter: MessageEventAdapter)

}



