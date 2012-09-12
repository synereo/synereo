package com.protegra_ati.agentservices.core.schema

import behaviors.Tracking
import scala.reflect.BeanProperty
import java.util.UUID

//TODO: perhaps combine fromAlias and fromBusinessName into BusinessCard? instead of using Profile and BusinessProfile
//@BeanProperty fromAlias: String,
//@BeanProperty fromBusinessName: String,

case class MockPost(
  @BeanProperty subject: String,
  @BeanProperty body: String,
  @BeanProperty toDetails: java.util.HashMap[ String, AnyRef ],
  @BeanProperty fromDetails: java.util.HashMap[ String, AnyRef ],
  @BeanProperty threadId: String)
  extends Data with Tracking
{

  def this() = this("", "", new java.util.HashMap(), new java.util.HashMap(), "")

  def this(_subject: String, _body: String, _fromDetails: java.util.HashMap[ String, AnyRef ]) = this(_subject, _body, new java.util.HashMap[ String, AnyRef ](), _fromDetails, UUID.randomUUID().toString())

  def this(_subject: String, _body: String, _toDetails: java.util.HashMap[ String, AnyRef ], _fromDetails: java.util.HashMap[ String, AnyRef ]) = this(_subject, _body, _toDetails, _fromDetails, UUID.randomUUID().toString())

}
