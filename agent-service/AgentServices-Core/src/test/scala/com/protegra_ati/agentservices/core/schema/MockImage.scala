package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.lang.reflect.Field

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
case class MockImage(@BeanProperty var name: String, @BeanProperty var contentType: String, @BeanProperty var content: String, @BeanProperty var metadata: String ) extends MockData
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
  def this(name: String, contentType: String, byteContent: Array[ Byte ], isChunked: Boolean, metadata: String ) =
    this(name, contentType, Image.toBase64String(byteContent), metadata)


}

case object MockImage
{
  final val EMPTY_IMAGE = Image("", "", "", "")
  final val JPEG_MIME_TYPE = "image/jpeg"
  final val PNG_MIME_TYPE = "image/png"
  final val simpleDemoMockImage = new MockImage("mockImage", PNG_MIME_TYPE, "IMAGE CONTENT WOULD BE HERE", "test metadata")

}







