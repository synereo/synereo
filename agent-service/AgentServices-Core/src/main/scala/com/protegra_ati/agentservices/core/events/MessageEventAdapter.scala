/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.events
//these are used as anonymous inner classes in java, should not be  with java.io.Serializable
//even though compile time allows run time will try to serialize whole outer object that may not be serializable

//strictly for java api compatibility, using the trait directly is compiled as interface
class MessageEventAdapter(_eventTag:String) extends MessageEventListener{
  def this() = this("")
  var eventTag: String = _eventTag
}