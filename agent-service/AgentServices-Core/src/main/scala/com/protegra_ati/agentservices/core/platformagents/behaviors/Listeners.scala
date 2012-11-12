/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.events._
import java.util.UUID
import java.util.HashMap
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util.{Severity, Reporting}
import java.util
import scala.collection.JavaConversions._
//import com.sun.org.apache.xpath.internal.operations._

import com.protegra_ati.agentservices.core.util._

/*
USE CASES
---------
uniqueness is determined primary by agentSession Key and then by subKey + eventTag
1. Listener1 = agentSession key, "" subkey, "" eventTag
  Header finds profile and says "welcome Jason"
2. Listener1= agentSession key, "header" subkey, "Profile" eventTag
   Listener2= agentSession key, "editPage" subkey, "Profile" eventTag
   Header uses profile to say "welcome Jason" should update when Edit Profile makes a change to "Jason2"
   Edit Profile page modifies profile and should ajax update when async saved
3. A single page is called many times to perform the same method, adding a listen each time. easy to forget to remove listener, we should prevent duplicates
   Listener1= agentSession key, "search" subkey, "Search" eventTag
   Listener2= agentSession key, "search" subkey, "Search" eventTag
   Listener3= agentSession key, "search" subkey, "Search" eventTag
   Only 1 listener should exist otherwise it looks like multiple results are returned
*/

trait Listeners extends Reporting
{
  final val LISTENER_PREFIX = "L"
  final val UNIQUE_PREFIX = "U"
  var _listeners = new MultiCacheMap[ MessageEventAdapter ](LISTENER_PREFIX)
  var _uniqueness = new MultiCacheMap[ String ](UNIQUE_PREFIX)

  //not providing a method with only key, listener. it should be a conscious choice to manage listeners per page
  def addListener(key: UUID, subKey: String, listener: MessageEventAdapter) =
  {
    report("in addListener - adding listener with key: " + key.toString + "for subkey" + subKey + " and eventTag: " + listener.eventTag)
    val keyUnique = subKey + listener.eventTag
    if ( !_uniqueness.hasValue(key.toString, keyUnique) ) {
      _listeners.add(key.toString, listener)
      _uniqueness.add(key.toString, keyUnique)
    }
    else {
      report("key + subkey must be unique, this pair already exists. No listener added", Severity.Warning)
    }
  }

  def removeListener(key: UUID, subKey: String, listener: MessageEventAdapter) =
  {
    report("in removeListener - removing listener with key: " + key.toString + "for subkey" + subKey + " and eventTag: " + listener.eventTag)
    val keyUnique = subKey + listener.eventTag
    _listeners.remove(key.toString, listener)
    _uniqueness.remove(key.toString, keyUnique)
  }

  def getListenersByMessage(msg: Message): List[ MessageEventAdapter ] =
  {
    //&& !msg.eventKey.eventTag.equals("")
    if ( msg.eventKey != null) {
      val matching = _listeners.get(msg.eventKey.agentSessionId.toString)
      matching match {
        case x: util.ArrayList[ MessageEventAdapter ] => {
          val filtered = x.filter(l => l.eventTag == msg.eventKey.eventTag)
          filtered.toList
        }
        case _ => { report("no one listening for this message event key " + msg.eventKey)
          List[ MessageEventAdapter ]()
        }
      }
    }
    else {
      report("in getListenersByMessage - message event not valid")
      Nil
    }
  }

  def triggerEvent(event: MessageEvent[ _ <: Message ])
  {
    report("in triggerEvent - triggering event for message type: " + event.msg.getClass.getName + "msg id: " + event.msg.ids.id.toString + " with agentSessionId: " + event.msg.eventKey.agentSessionId.toString + " and eventTag: " + event.msg.eventKey.eventTag)
    event.trigger(getListenersByMessage(event.msg))
  }

}
