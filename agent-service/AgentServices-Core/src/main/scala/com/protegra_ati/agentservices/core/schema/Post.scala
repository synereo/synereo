package com.protegra_ati.agentservices.core.schema

import behaviors.Tracking
import behaviors.Archive
import behaviors.Ignore
import scala.reflect.BeanProperty
import org.joda.time.DateTime
import java.util.UUID
import scala.collection.JavaConversions._
import java.util.HashMap

//TODO: perhaps combine fromAlias and fromBusinessName into BusinessCard? instead of using Profile and BusinessProfile
//@BeanProperty fromAlias: String,
//@BeanProperty fromBusinessName: String,

case class Post(
  @BeanProperty var subject: String,
  @BeanProperty var body: String,
  @BeanProperty var toDetails: java.util.HashMap[String, Data],
  @BeanProperty var fromDetails: java.util.HashMap[String, Data],
  @BeanProperty var threadId: String)
  extends Data
  with Tracking
  with Archive
  with Ignore
  with UINotifiable
{

  def this() = this("", "", new java.util.HashMap(), new java.util.HashMap(), "")
  def this(_subject: String, _body: String, _fromDetails: java.util.HashMap[String, Data]) = this(_subject, _body, new java.util.HashMap(), _fromDetails, UUID.randomUUID().toString())
  def this(_subject: String, _body: String, _toDetails: java.util.HashMap[String, Data], _fromDetails: java.util.HashMap[String, Data]) = this(_subject, _body, _toDetails, _fromDetails, UUID.randomUUID().toString())

  override protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] = List("body","toDetails","fromDetails") ::: super.ignoredFieldsForSearchAndStoreKey
}

object Post
{

  final val SEARCH_ALL_KEY = new Post().toSearchKey

  final val SEARCH_ALL = new Post()
  {
    override def toSearchKey(): String = Post.SEARCH_ALL_KEY
  }

}
