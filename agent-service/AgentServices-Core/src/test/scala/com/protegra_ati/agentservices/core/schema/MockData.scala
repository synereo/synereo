package com.protegra_ati.agentservices.core.schema

import java.io.Serializable
import java.lang.reflect._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.schema.util.ReflectionHelper._
import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.extensions._
import com.protegra_ati.agentservices.core.schema.Constants._
import java.util.HashMap
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization


class MockData(_id: String, _localeCode: String, _classVersionNumber: String) extends Serializable  with UseKryoSerialization
{
  def this(_id: String, _localeCode: String) = this(_id, _localeCode, MockData.currentVersion)

  def this() = this("", "", MockData.currentVersion)

  @BeanProperty
  var id: String = _id

  @BeanProperty
  var localeCode: String = _localeCode
  @BeanProperty
  val classVersionNumber = _classVersionNumber

  def className: String =
  {
    this.getClass.getName
  }


  //if this is val it shows up on toStoreKey/toSearchKey
  //important to note that formattedClassName matches up to DisclosedData.dataDisplayClassName
  def formattedClassName: String =
  {
    this.getClass.getName.trimPackage.toCamelCase
  }

  def formattedClassName(classType: Class[ _ <: MockData ]): String =
  {
    classType.getName.trimPackage.toCamelCase
  }

}

object MockData
{
  def currentVersion: String = "1.1"

}
