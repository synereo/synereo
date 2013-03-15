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
import com.protegra_ati.agentservices.store.util.{MultiMap, MemCache, Severity, Reporting}
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
  @transient var _listeners = new MultiMap[ UUID, MessageEventAdapter ]
  var _uniqueness = new MultiMap[ UUID, String ]

  //not providing a method with only key, listener. it should be a conscious choice to manage listeners per page
  def addListener(key: UUID, subKey: String, listener: MessageEventAdapter) =
  {
    report("in addListener - adding listener with key: " + key.toString + "for subkey" + subKey + " and eventTag: " + listener.eventTag)
    val keyUnique = buildKeyUnique(listener.eventTag, subKey)
    if (!_uniqueness.hasValue(key, keyUnique))
    {
      _listeners.add(key, listener)
      _uniqueness.add(key, keyUnique)
    }
    else
    {
      report("key + subkey must be unique, this pair already exists. No listener added", Severity.Warning)
    }
  }

  def removeListenersByTag(key: UUID, tag: String) =
  {
    report("in removeListenersByTag - removing listener with key: " + key.toString + " for tag " + tag)
    val listeners = getListenersByTag(key, tag)
    listeners.foreach(l => {
      _listeners.remove(key, l)
    })

    val unique = _uniqueness.get(key)
    unique.foreach(u => {
      if(u.startsWith(tag + "-")) {
        _uniqueness.remove(key, u)
      }
    })
  }

  def buildKeyUnique(eventTag: String, subKey: String): String =
  {
    eventTag + "-" + subKey
  }


  def removeListener(key: UUID, subKey: String, listener: MessageEventAdapter) =
  {
    report("in removeListener - removing listener with key: " + key.toString + "for subkey" + subKey + " and eventTag: " + listener.eventTag)
    val keyUnique = buildKeyUnique(listener.eventTag, subKey)
    _listeners.remove(key, listener)
    _uniqueness.remove(key, keyUnique)
  }

  def getListenersByMessage(msg: Message): List[ MessageEventAdapter ] =
  {
//    (  && !msg.eventTag.equals("") )
    if ( msg.eventKey != null ) {
      val matching = _listeners.get(msg.eventKey.agentSessionId)
      matching match {
        case null => List[ MessageEventAdapter ]()
        case _ => {
          val filtered = matching.filter(l => l.eventTag == msg.eventKey.eventTag)
          filtered
        }
      }
    }
    else {
      report("in getListenersByMessage - message event not valid")
      Nil
    }
  }

  def getListenersByTag(key: UUID, tag: String): List[ MessageEventAdapter ] =
  {
    if (tag != null) {
      val matching = _listeners.get(key)
      matching match {
      case null => List[ MessageEventAdapter ]()
      case _ => {
        val filtered = matching.filter(l => l.eventTag == tag)
        filtered
      }
    }
  }
  else {
    report("in getListenersByTag - tag not valid")
    Nil
  }
  }

  def triggerEvent(event : MessageEvent[_ <: Message])
  {
    try
    {
      report("in triggerEvent - triggering event for message type: " + event.msg.getClass.getName + "msg id: " + event.msg.ids.id.toString + " with agentSessionId: " + event.msg.eventKey.agentSessionId.toString + " and eventTag: " + event.msg.eventKey.eventTag)
      event.trigger(getListenersByMessage(event.msg))
    } catch {
      case e: Exception => {
        report("problem in triggerEvent", Severity.Error)
        e.printStackTrace()
      }
      case _ => {}
    }
  }

}
