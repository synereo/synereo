package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: mgevantmakher
*/

import org.joda.time.{DateTime, Instant}
import com.protegra_ati.agentservices.core.messages._
import scala.collection.mutable.Map
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.util._

//TODO create the store and save references to the CreateInvitationRequests
//     check if the Response fits in a class InvitationRequestSetConsumer. if it is so than  autoaprove
//if not than store the message and goes to the step 3a(j)

trait MessageStore extends Reporting
{

  self: AgentHostStorePlatformAgent =>
  val DELIMITER = "++"
  val storage: Map[ String, DateTime ] = Map[ String, DateTime ]()
  /**
   * message time to life (milliseconds)
   */
  val ONE_HOUR_TTL = 3600000

  def capture(cnxn: AgentCnxnProxy, requestMsg: Message) = synchronized {
    val storageMoment = new DateTime()
    report("MESSAGE TEMPORARY STORED: " + cnxn + ", message ids=" + requestMsg.ids + " AT " + storageMoment, Severity.Trace)
    // key can be reduced to id + snxn.src
    storage += ( requestMsg.ids.conversationId + DELIMITER + cnxn.src.toString + DELIMITER + cnxn.trgt.toString -> storageMoment )
  }

  protected def isCaptured(messageConversationId: String, srcUID: String, targetUID: String): Boolean = synchronized {
    storage.contains(messageConversationId + DELIMITER + srcUID + DELIMITER + targetUID)
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
    storage.get(key) match {
      case None => {
        report("can't find captured message message conversationID=" + responseMsg.ids.conversationId + ", srcUID=" + cnxn.trgt.toString + ", targetUID=" + cnxn.src.toString, Severity.Trace)
        false
      }
      case Some(storageMoment) => {
        if ( !( storageMoment.isBefore(new DateTime().minus(beforeNowWithinMilliseconds)) ) )
          true
        else false
      }
      false
    }
  }

  // TODO thread safe auto cleanup there is already great solution by MG in his SCJD project
}