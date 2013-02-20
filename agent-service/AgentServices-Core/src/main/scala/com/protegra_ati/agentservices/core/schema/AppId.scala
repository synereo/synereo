package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.util.{UUID}
import scala.collection.JavaConversions._
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import com.protegra_ati.agentservices.core.schema.persistence.CacheableData

//id will come from data
//concrete instance until we come up with a better way of DisclosedData, right now it's by class
//ideally a generic AppId but not one that's seen by other apps
case class AppId(@BeanProperty val name: String,@BeanProperty var policies: java.util.List[ String ])
  extends CacheableData 
  //TODO: with ExcludeFromAudit
{

  def this(
    _name: String) = this(_name, Nil)

  def this() = this("")

  override def toCacheObject(appId: String): DBObject =
  {
    val o = MongoDBObject(
      "dataId" -> id,
      "appId" -> appId,
      "name" -> name,
      "policies" -> policies
    )

    o
  }

  override def fromDBObject(o: DBObject): CacheableData = 
  {
    new AppId()
  }

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


}

object AppId
{
  final val SEARCH_ALL_KEY = new AppId().toSearchKey

  final val SEARCH_ALL = new AppId()
  {
    override def toSearchKey(): String = AppId.SEARCH_ALL_KEY
  }
}
