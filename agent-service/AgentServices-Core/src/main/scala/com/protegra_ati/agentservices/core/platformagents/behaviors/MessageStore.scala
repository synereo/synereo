package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: mgevantmakher
*/

import org.joda.time.{DateTime, Instant}
import com.protegra_ati.agentservices.core.messages._
import scala.collection.mutable.Map
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util._
import com.protegra_ati.agentservices.core.util.Results

//TODO create the store and save references to the CreateInvitationRequests
//     check if the Response fits in a class InvitationRequestSetConsumer. if it is so than  autoaprove
//if not than store the message and goes to the step 3a(j)

trait MessageStore extends Reporting
{

  self: AgentHostStorePlatformAgent =>
  val DELIMITER = "++"
  /**
   * message time to life (milliseconds)
   */
  val ONE_HOUR_TTL = 3600000

  def capture(cnxn: AgentCnxnProxy, requestMsg: Message) = synchronized {
    val storageMoment = new DateTime()
    report("MESSAGE TEMPORARY STORED: " + cnxn + ", message ids=" + requestMsg.ids + " AT " + storageMoment, Severity.Trace)
    // key can be reduced to id + snxn.src
    val key = requestMsg.ids.conversationId + DELIMITER + cnxn.src.toString + DELIMITER + cnxn.trgt.toString
    MemCache.set(key, storageMoment, 0)(Results.client)
  }

  protected def isCaptured(messageConversationId: String, srcUID: String, targetUID: String): Boolean = synchronized {

   val key = messageConversationId + DELIMITER + srcUID + DELIMITER + targetUID
   val found = MemCache.get[ DateTime ](key)(Results.client)
   found != null
  }

  def isCaptured(cnxn: AgentCnxnProxy, responseMsg: Message): Boolean = synchronized {
    //in case of response  AgentCnxnProxy has reverse order of the src and target point
    isCaptured(responseMsg.ids.conversationId, cnxn.trgt.toString, cnxn.src.toString)
  }

  /**
   * checks if a messag was captured within "beforeNowWithinMilliseconds"-value of millisec.
   * @param cnxn
   * @param responseMsg
   * @param beforeNowWithinMilliseconds  time to life of the message
   * @return true if captured within given period of time
   */
  def isCaptured(cnxn: AgentCnxnProxy, responseMsg: Message, beforeNowWithinMilliseconds: Long): Boolean = synchronized {
    //in case of response  AgentCnxnProxy has reverse order of the src and target point
    val key = responseMsg.ids.conversationId + DELIMITER + cnxn.trgt.toString + DELIMITER + cnxn.src.toString
    val dateCaptured = MemCache.get[ DateTime ](key)(Results.client)
    dateCaptured match {
      case null => {
        report("can't find captured message message conversationID=" + responseMsg.ids.conversationId + ", srcUID=" + cnxn.trgt.toString + ", targetUID=" + cnxn.src.toString, Severity.Trace)
        false
      }
      case storageMoment:DateTime => {
        if ( !( storageMoment.isBefore(new DateTime().minus(beforeNowWithinMilliseconds)) ) )
          true
        else false
      }
      false
    }
  }

  // TODO thread safe auto cleanup there is already great solution by MG in his SCJD project
}