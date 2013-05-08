package com.ati.iaservices.messages.referral

import com.protegra_ati.agentservices.core.schema.Post
import scala.beans.BeanProperty
import java.util.UUID

case class Referral (@BeanProperty val invitationConnectionId: UUID,
                     @BeanProperty val alias: String,
                     @BeanProperty val post: Post)
