package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.lang.reflect.Field
import java.util.Properties

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
  def this() = this(null, null, null.asInstanceOf[ String ], null)

  /**
   *  constructor
   * @param name  name of the image
   * @param contentType mime type of the content according to a IANA official list from http://www.iana.org/assignments/media-types/index.html. Use some declared in th object Image
   * @param byteContent some image file as byte array. Do not use a null a argument
   * @param metadata keywords describing an image, for search purpose
   * @return
   */
  def this(name: String, contentType: String, byteContent: Array[ Byte ], metadata: String) =
    this(name, contentType, Image.toBase64String(byteContent), metadata)


  /**
   *  Deprecated, use instead constructor with (name: String, contentType: String, byteContent: Array[ Byte ], metadata: String)
   * @param name  name of the image
   * @param contentType mime type of the content according to a IANA official list from http://www.iana.org/assignments/media-types/index.html. Use some declared in th object Image
   * @param byteContent some image file as byte array. Do not use a null a argument
   * @param isChunked This parameter has no effect, since new base64 API is used for encoding and decoding
   * @param metadata keywords describing an image, for search purpose
   * @return
   */
  @deprecated
  def this(name: String, contentType: String, byteContent: Array[ Byte ], isChunked: Boolean, metadata: String) =
    this(name, contentType, byteContent, metadata)


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
  //STRESS TODO use as fare as possible this constant in profile or business profile objects used for search (UI driven)!!!


  private final val emptyByteArray = Array[ Byte ](0)
  private final val emptyString = ""

  final val SEARCH_ALL_KEY = new Image().toSearchKey
  final val SEARCH_ALL = new Image()
  {
    override def toSearchKey(): String = Image.SEARCH_ALL_KEY
  }

  final val JPEG_MIME_TYPE = "image/jpeg"
  final val PNG_MIME_TYPE = "image/png"


  def toBase64String(byteContent: Array[ Byte ]): String =
  {
    if ( byteContent == null ) return null.asInstanceOf[ String ]
    if ( byteContent.isEmpty ) return emptyString
    new String(biz.source_code.base64Coder.Base64Coder.encode(byteContent))
  }


  @deprecated
  def toBase64String(byteContent: Array[ Byte ], isChunked: Boolean): String =
  {toBase64String(byteContent)}

  def toByteArray(base64Encoded: String): Array[ Byte ] =
  {
    if ( base64Encoded == null ) return null.asInstanceOf[ Array[ Byte ] ]
    if ( base64Encoded.isEmpty ) return emptyByteArray

    biz.source_code.base64Coder.Base64Coder.decode(base64Encoded)
  }

def fromProperty(src: Properties, parent: String): Image =
   {
     new Image(
       src.getProperty(parent + "." + "name"),
       src.getProperty(parent + "." + "contentType"),
       src.getProperty(parent + "." + "content"),
       src.getProperty(parent + "." + "metadata")
     )
   }

}







