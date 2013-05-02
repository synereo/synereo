package com.ati.iaservices.schema

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.schema.behaviors.Tracking

class LabelKey(var searchKey: String) extends Data {
  def this() = this("_")

  override protected def rawFieldsForSearchAndStoreKey(): List[String] = List("searchKey") ::: super.rawFieldsForSearchAndStoreKey

  override def toString: String = {
    {
      String.format("LabelKey[id=%s, locale=%s, searchKey=%s]", id, localeCode, searchKey)
    }
  }
}

object LabelKey {
  final val SEARCH_ALL_KEY = new LabelKey().toSearchKey

  final val SEARCH_ALL = new LabelKey() {
    override def toSearchKey: String = LabelKey.SEARCH_ALL_KEY
  }
}

case class Label(var key: LabelKey, var content: Content) extends Data
with StorableLabelDataDefaults
with Tracking {
  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this(new LabelKey(), null.asInstanceOf[Content])

  var created: DateTime = null
  var contentType = formattedContentValueName

  def formattedContentValueName: String = {
    if (content == null) {
      ""
    }
    else {
      content.getClass.getName.trimPackage
    }
  }

  override protected def ignoredFieldsForSearchAndStoreKey(): List[String] = List("content") ::: super.ignoredFieldsForSearchAndStoreKey

  override def toString: String = {
    {
      String.format("Label[id=%s, locale=%s, key=%s, content=%s, created=%s]", id, localeCode, key, content, (if (created == null) "null" else created))
    }
  }
}

object Label {
  final val SEARCH_ALL_KEY = new Label().toSearchKey

  final val SEARCH_ALL = new Label() {
    override def toSearchKey: String = Label.SEARCH_ALL_KEY
  }
}

