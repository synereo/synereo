package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.lang.reflect.Field
import org.apache.commons.codec.binary.Base64;

/* User: mgevantmakher
*/

/**
 * Images used in app for avatar- pictures/logos etc.
 * @param name name of the image
 * @param contentType mime type of the content according to a IANA official list from http://www.iana.org/assignments/media-types/index.html. Use some declared in th object Image
 * @param content some image file as base64 encoded byte array
 * @param metadata keywords describing an image, for search purpose
 */
//case class Image(@BeanProperty name: String, @BeanProperty contentType: String, @BeanProperty content: Array[ Byte ], @BeanProperty metadata: String) extends Data
case class Image(@BeanProperty name: String, @BeanProperty contentType: String, @BeanProperty content: String, @BeanProperty metadata: String) extends Data
{
  def this() = this(null, null, null, null)

  /**
   *
   * @param name  name of the image
   * @param contentType mime type of the content according to a IANA official list from http://www.iana.org/assignments/media-types/index.html. Use some declared in th object Image
   * @param byteContent some image file as byte array. Do not use a null a argument
   * @param isChunked if <code>true</code> this encoder will chunk the base64 output into 76 character blocks, use false by default
   * @param metadata keywords describing an image, for search purpose
   * @return
   */
  def this(name: String, contentType: String, byteContent: Array[ Byte ], isChunked: Boolean, metadata: String) =
    this(name, contentType, Image.toBase64String(byteContent, isChunked), metadata)

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

  /**
   * returns encoded byte array of image file
   * @return  Array[ Byte ] can be used to show the picture
   */
  def getContentAsByteArray(): Array[ Byte ] =
  {Image.toByteArray(content)}

  override def equals(other: Any): Boolean =
  {
    var result = false
    other match {
      case that: Image => {
        result = ( that canEqual this ) &&
          this.name == that.name && this.contentType == that.contentType && this.metadata == that.metadata // && this.sameContent(that.content)
      }
      case _ =>
        result = false
    }
    result
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

  override def canEqual(other: Any) =
    other.isInstanceOf[ Image ]
}

case object Image
{
  final val EMPTY_IMAGE = Image("", "", "", "")
  final val JPEG_MIME_TYPE = "image/jpeg"
  final val PNG_MIME_TYPE = "image/png"


  def toBase64String(byteContent: Array[ Byte ], isChunked: Boolean): String =
  {new String(Base64.encodeBase64(byteContent, isChunked))}

  def toByteArray(base64Encoded: String): Array[ Byte ] =
  {Base64.decodeBase64(base64Encoded.getBytes())}
}







