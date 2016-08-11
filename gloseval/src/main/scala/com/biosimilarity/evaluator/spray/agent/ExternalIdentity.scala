package com.biosimilarity.evaluator.spray.agent

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

//@@GS thought bubble
// note this does _not_ currently include 'ANY'
trait ExternalIdType
trait SocialIdType extends ExternalIdType

case object ID_EMAIL extends ExternalIdType {
  override def toString = "Email"
}
case object ID_SMS extends ExternalIdType {
  override def toString = "SMS"
}
case object ID_FACEBOOK extends SocialIdType {
  override def toString = "Facebook"
}
case object ID_GITHUB extends SocialIdType {
  override def toString = "GitHub"
}

// ETC. ETC.
object ExternalIdType {
    def fromString(s : String) : ExternalIdType = {
      // GS - naive in the extreme (still on L-plates ...)
      s.toUpperCase match {
        case "EMAIL" => ID_EMAIL
        case "SMS" => ID_SMS
        case "FACEBOOK" => ID_FACEBOOK
        case "GITHUB" => ID_GITHUB
      }
    }
}

case class ExternalIdentity (idType: ExternalIdType, idValue: String )

