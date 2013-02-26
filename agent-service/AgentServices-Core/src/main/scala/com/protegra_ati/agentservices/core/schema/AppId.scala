package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.util.{UUID}
import scala.collection.JavaConversions._
import com.mongodb.casbah.Imports._
import com.protegra_ati.agentservices.core.schema.persistence.CacheableData
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

//id will come from data
//concrete instance until we come up with a better way of DisclosedData, right now it's by class
//ideally a generic AppId but not one that's seen by other apps
case class AppId(
  @BeanProperty var name: String,
  @BeanProperty var policies: java.util.List[ String ]
  )
  extends CacheableData
  //TODO: with ExcludeFromAudit
{
  def this(_name: String) = this(_name, Nil)

  def this() = this("", Nil)

  override def toCacheObject(): DBObject =
  {
    val o = MongoDBObject(
      "name" -> name,
      "policies" -> MongoDBList.concat(policies)
    )

    super.toCacheObject ++ o
  }

  override def fromDBObject(o: DBObject): CacheableData = 
  {
    val policies = o.getAsOrElse[MongoDBList]("policies", MongoDBList.empty).toList.collect { case s:String => s}

    val appId = new AppId(
      o.getAsOrElse("name", ""),
      policies
    )

    appId.addIds(o.getAsOrElse("dataId", ""), "", o.getAsOrElse("brokerCnxnAppId", ""), o.getAsOrElse("brokerCnxnExchangeKey", ""))

    appId
  }

  // Find all AppIds current AppId is connected to (i.e. current AppId is set as brokerCnxnAppId)
  def connectedAppIdsQuery(): DBObject = MongoDBObject("brokerCnxnAppId" -> id)

  def setPoliciesFromCategory(category: String): Unit =
  {
    if ( category == ConnectionCategory.Group.toString ) {
       policies = AppIdPolicy.GroupProfileEditable.toString :: Nil
    }
    else if ( category == ConnectionCategory.Person.toString ) {
       policies = AppIdPolicy.ProfileEditable.toString :: Nil
    }
    else if ( category == ConnectionCategory.Business.toString ) {
      policies = AppIdPolicy.BusinessProfileEditable.toString :: Nil
    }
    else if ( category == ConnectionCategory.Affiliate.toString ) {
      policies = AppIdPolicy.ProfileEditable.toString :: AppIdPolicy.BusinessProfileEditable.toString :: Nil
    }
  }

  override def getMongoCollectionName:String = AppId.MONGO_COLLECTION_NAME
}

object AppId
{
  final val SEARCH_ALL_KEY = new AppId().toSearchKey

  final val MONGO_COLLECTION_NAME = AppId.getClass.getName.trimPackage.toCamelCase

  final val SEARCH_ALL = new AppId()
  {
    override def toSearchKey(): String = AppId.SEARCH_ALL_KEY
  }
}
