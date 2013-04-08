package com.ati.iaservices.schema

import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.Data

case class Label (
  @BeanProperty val name: String,
  @BeanProperty val parentLabelId: Option[String]
  ) extends Data {

  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("", None)

}

object Label
{

  final val SEARCH_ALL_KEY = new Label().toSearchKey

  final val SEARCH_ALL = new Label()
  {
    override def toSearchKey(): String = Label.SEARCH_ALL_KEY
  }

}

