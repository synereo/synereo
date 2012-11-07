/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.events

//strictly for java api compatibility, using the trait directly is compiled as interface
class MessageEventAdapter(_eventTag:String) extends MessageEventListener with java.io.Serializable {
  def this() = this("")
  var eventTag: String = _eventTag
}