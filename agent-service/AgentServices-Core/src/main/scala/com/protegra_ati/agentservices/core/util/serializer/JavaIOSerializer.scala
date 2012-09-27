package com.protegra_ati.agentservices.core.util.serializer

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import org.apache.ws.commons.util.Base64
import com.protegra.agentservicesstore.util._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions._
import java.util.UUID
// TODO reporting ha to eventually reaktivated
class JavaIOSerializer extends AbstractToStringSerializer //with Reporting
{
  private val HEADER_4_STRING_SERIALIZATION = "JavaIOSerializer_"

  protected def serializeRaw(objToBeSerialized: Object): String =
  {
    val uid5CharLong = UUID.randomUUID().toString().substring(0, 5)
   // System.err.println("#####-serialize JAVA IO--ID:" + uid5CharLong + "--: " + objToBeSerialized);

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(objToBeSerialized)
    val encodedMsg = new String(Base64.encode(baos.toByteArray))
    oos.close
    sizeWarning(encodedMsg, objToBeSerialized)
    return uid5CharLong + encodedMsg
  }

  def getHeader(): String =
  {
    // returns always the type
    return HEADER_4_STRING_SERIALIZATION;
  }

  def sizeWarning(encodedMsg: String, obj: Object) =
  {
    val bytesInAKilobyte = 1024
    val maxBytes = 100 * bytesInAKilobyte
    //each char roughly 1 byte
   // if ( encodedMsg.size > maxBytes )
      //report("serialized message is more than " + maxBytes / bytesInAKilobyte + " KB for obj " + obj.toString, Severity.Warning)
  }

  def deserializeRaw[ T ](source: String): T =
  {
    try {
      val uid = source.substring(0, 5);
      val byteArrayMsg = Base64.decode(source.substring(5, source.length()))
      val ois = new ObjectInputStream(new ByteArrayInputStream(byteArrayMsg))
      val resultMsg = ois.readObject
      ois.close
     // System.err.println("#####-deserialize JAVA IO--ID:" + uid + " ----: " + resultMsg);
      resultMsg match {
        case x: T => return x
        case _ => return null.asInstanceOf[ T ]
      }
    }
    catch {
      case _ => {} // report("Failed to deserialize the class, have you recently upgraded the scala version without recreating test data?", Severity.Fatal)
      return null.asInstanceOf[ T ]
    }

  }
}

object JavaIOSerializer
{

  private val javaIoSerializer: JavaIOSerializer = new JavaIOSerializer()

  def getInstance(): JavaIOSerializer = javaIoSerializer


}
