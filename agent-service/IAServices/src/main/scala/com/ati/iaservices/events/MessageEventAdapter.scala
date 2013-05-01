/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.ati.iaservices.events

import com.protegra_ati.agentservices.core.events.{MessageEventAdapter => CoreMessageEventAdapter}

//strictly for java api compatibility, using the trait directly is compiled as interface
class MessageEventAdapter(_eventTag:String) extends CoreMessageEventAdapter(_eventTag) with MessageEventListener {
  def this() = this("")
}