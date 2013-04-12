package com.protegra_ati.agentservices.core.util

import java.io._
import org.apache.commons.io.FileUtils
import com.protegra_ati.agentservices.store.util.{Reporting, Severity}

/* User: mgevantmakher
*/

object Base64APIsComparison extends Reporting
{


  val AVATAR_IMAGE_BYTE_REPRESENTATION = loadFromFile(new File("BaseXData_01102012_7_large.zip"))

  def main(arg: Array[ String ]) =
  {
   // 683 approachSunMiscBASE64()
   // 152 approachOrgApacheCommonsCodecBinaryBase64()
     approachBizSource_codeBase64CoderBase64()
  }

  def approachSunMiscBASE64() =
  {
    val start = System.currentTimeMillis()

    try {
      // Convert a byte array to base64 string
      val initialByteArray = AVATAR_IMAGE_BYTE_REPRESENTATION

      val s: String = new sun.misc.BASE64Encoder().encode(initialByteArray)

      // Convert base64 string to a byte array
      val res1 = new sun.misc.BASE64Decoder().decodeBuffer(s)

      if ( !initialByteArray.sameElements(res1) ) {
              throw new IOException("sun.misc.BASE64 approach doesn't work")
            }

      val end = System.currentTimeMillis()
      println("sun.misc.BASE64 approach: " + ( end - start ))
    } catch {
      case e: Exception => report("Exception occured in serialize method", e, Severity.Error)
    }

  }

  def approachOrgApacheCommonsCodecBinaryBase64() =
  {
    val start = System.currentTimeMillis()
    val initialByteArray = AVATAR_IMAGE_BYTE_REPRESENTATION
    try {
      val encoded = new String(org.apache.commons.codec.binary.Base64.encodeBase64(initialByteArray));

      val res1 = org.apache.commons.codec.binary.Base64.decodeBase64(encoded.getBytes());
      if ( !initialByteArray.sameElements(res1) ) {
        throw new IOException("org.apache.commons.codec.binary.Base64 approach doesn't work")
      }
      val end = System.currentTimeMillis()
      println("org.apache.commons.codec.binary.Base64 approach: " + ( end - start ))
    } catch {
      case e: Exception => report("Exception occured in serialize method", e, Severity.Error)
    }

  }


  def approachBizSource_codeBase64CoderBase64() =
  {
    val start = System.currentTimeMillis()
    val initialByteArray = AVATAR_IMAGE_BYTE_REPRESENTATION
    try {

      val encoded = new String(biz.source_code.base64Coder.Base64Coder.encode(initialByteArray));

      val res1 = biz.source_code.base64Coder.Base64Coder.decode(encoded);
      if ( !initialByteArray.sameElements(res1) ) {
        throw new IOException("biz.source_code.base64Coder.Base64Coder approach doesn't work")
      }
      val end = System.currentTimeMillis()
      println("biz.source_code.base64Coder.Base64Coder approach: " + ( end - start ))
    } catch {
      case e: Exception => report("Exception occured in serialize method", e, Severity.Error)
    }

  }

  def loadFromFile(f: File): Array[ Byte ] =
  {
    val bytes = FileUtils.readFileToByteArray(f)
    bytes.toArray[ Byte ]
  }

}


