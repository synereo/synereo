package com.protegra_ati.agentservices.core.schema

import java.util.UUID
import scala.reflect.BeanProperty
import org.joda.time.DateTime

case class AuditLogItem(@BeanProperty var detail:String, @BeanProperty var dateLogged:DateTime ) extends Data {
 def this() = this(null, null) //-- may be this is a better solution?
// def this() = this("", new DateTime()) wrong

}