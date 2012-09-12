package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.messages.EventKey

import java.util.UUID

// TODO may be to refactor to all string values
case class LoginToken(@BeanProperty var key:EventKey, @BeanProperty var currentUICnxn:Connection,  @BeanProperty var expirationDate:DateTime)extends Data{
  def this() = this(null, null, null)
}