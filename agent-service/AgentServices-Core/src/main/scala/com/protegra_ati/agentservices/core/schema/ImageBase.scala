package com.protegra_ati.agentservices.core.schema

import reflect.BeanProperty
import java.lang.reflect.Field
import java.util.Properties

abstract class ImageBase(@BeanProperty val name: String, @BeanProperty val contentType: String, @BeanProperty val content: String, @BeanProperty val metadata: String) extends Data {

  def this() = this("", "", "", "")




  override protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] =
  {List("content")}

  override def hashCode = 41 * super.hashCode + name.hashCode + contentType.hashCode + getContentLength + metadata.hashCode

  def getContentLength(): Int =
  {
    if ( content == null )
      0
    else
      content.length
  }





  override protected def getFormattedFieldValue(field: Field): String =
  {
    ( "\"" + getFieldValue(field) + "\"" )
  }


  def sameContent(thatContent: Array[ Byte ]): Boolean =
  {
    if ( content == null && thatContent == null )
      true
    else if ( content == null )
      false
    else
      content.sameElements(thatContent)
  }

}