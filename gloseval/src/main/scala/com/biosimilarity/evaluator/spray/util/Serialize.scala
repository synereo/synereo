package com.biosimilarity.evaluator.spray.util

object Serialize {
  import biz.source_code.base64Coder.Base64Coder
  import java.io._
  implicit def boxUnit(u: Unit):Either[java.io.Serializable,Unit] = Right(())
  implicit def boxSerializable(t: java.io.Serializable):Either[java.io.Serializable,Unit] = Left(t)
/*
  def deserialize[T <% Either[java.io.Serializable, Unit]](s: String): T = {
    s(0) match {
      case '0' => ()
      case '1' => {
        val data : Array[Byte] = Base64Coder.decode(s.substring(1))
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[T]
      }
      case _ => throw new Exception("Bad encoding")
    }
  }

  def serialize[T <% Either[java.io.Serializable, Unit]](t: T): String = {
    t match {
      case u: Unit => "0"
      case _ => {
        val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos : ObjectOutputStream = new ObjectOutputStream( baos )
        oos.writeObject( t.asInstanceOf[java.io.Serializable] )
        oos.close()
        "1" + new String( Base64Coder.encode( baos.toByteArray() ) )
      }
    }
  }
//*/
}
