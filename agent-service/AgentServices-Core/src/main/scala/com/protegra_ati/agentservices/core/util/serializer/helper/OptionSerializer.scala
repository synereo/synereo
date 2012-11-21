package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.Option


class OptionSerializer() extends Serializer[ Option[ AnyRef ] ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }

  override def write(kryo: Kryo, output: Output, obj: Option[ AnyRef ]): Unit =
  {
    if ( obj == null )
      kryo.writeObject(output, OptionSerializer.NULL)
    else {
      kryo.writeObject(output, OptionSerializer.NOT_NULL)

      try {
       // println("KRYO Option in USE! WRITE")
        obj match {
          case None => {
           // println("NONE")
            kryo.writeObject(output, OptionSerializer.NONE)
          }
          case Some(_) => {
           // println("SOME")
            kryo.writeObject(output, OptionSerializer.SOME)
            val someClass = cacheSomeValue(obj)
            kryo.writeObject(output, someClass.getName)
            kryo.writeObject(output, obj.get)
          }
          case _ => {
            println("KRYO Option in USE! WRITE PROBLEM!!!!")
          }
        }
      } catch {
        case ex: Exception => {
          ex.printStackTrace()

        }
      }
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ Option[ AnyRef ] ]): Option[ AnyRef ] =
  {
    //println("KRYO Option in USE! READ")
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == OptionSerializer.NULL ) return null

    val optionType: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( optionType == OptionSerializer.NONE )
      return None
    val someClass = kryo.readObject(input, classOf[ String ])
    return Some(kryo.readObject(input, Class.forName(someClass)).asInstanceOf[ AnyRef ])
  }

  private def cacheSomeValue(obj: Option[ AnyRef ]): Class[ _ ] =
  {
    val claz = obj.get.getClass
    return claz
  }


}

object OptionSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;
  val NONE: Byte = 3;
  val SOME: Byte = 4;

  def FITS_TO: Class[ Option[ AnyRef ] ] = classOf[ Option[ AnyRef ] ]
}
