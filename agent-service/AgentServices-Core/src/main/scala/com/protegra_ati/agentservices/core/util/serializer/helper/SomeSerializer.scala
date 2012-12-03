package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

// TODO has to be cleaned up
class SomeSerializer() extends Serializer[ Some[ AnyRef ] ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }


  override def write(kryo: Kryo, output: Output, obj: Some[ AnyRef ]): Unit =
  {
    //println("SERIALIZE SOME: " + obj)
    if ( obj == null )
      kryo.writeObject(output, SomeSerializer.NULL)
    else {
      //kryo.writeObject(output, SomeSerializer.NOT_NULL)

      // TODO eventually bug with none
      val someClass = cacheSomeValue(obj)

      if ( classOf[ scala.Enumeration$Val ].equals(someClass)
      /*scala.Enumeration$Val*/ ) {
        //println("SER KRYO SOME in USE! WRITE:  Enumeration$Value:" +obj.get)
        kryo.writeObject(output, SomeSerializer.NOT_NULL_ENUM)
        kryo.writeClassAndObject(output, obj.get)
       // kryo.writeObject(output, obj.get, new EnumerationSerializer())
      }
      else {
        //println("SER KRYO SOME in USE! WRITE:  SOMETHING BUT NOT ENUM: " + someClass.getName)
        kryo.writeObject(output, SomeSerializer.NOT_NULL_NOT_ENUM)
        kryo.writeObject(output, someClass.getName)
        kryo.writeObject(output, obj.get)
      }

      // if ( someClass == None ) println("KRYO SOME in USE! WRITE: NONE")
      //else
      //println("SER KRYO SOME in USE! WRITE: " + someClass.getName)


      //}
      //kryo.writeObject(output, obj.get)
      //    if ( someClass == scala.Enumeration$Val ) {
      //      println("KRYO IN SOME ENUM WRITE")
      //    }
      //    if ( obj.get.isInstanceOf[ scala.Enumeration#Value ] )
      //      kryo.writeObject(output, obj.get.asInstanceOf[ scala.Enumeration#Value ])//, new EnumerationSerializer())
      //    else

    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ Some[ AnyRef ] ]): Some[ AnyRef ] =
  {
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == SomeSerializer.NULL ) return null
    else if ( label == SomeSerializer.NOT_NULL_ENUM ) {
      val o =  kryo.readClassAndObject(input)
      //println("DESER KRYO SOME ENUM in USE! READ:"+ o)
      return Some(o)
    }
    else {
      val someClass = kryo.readObject(input, classOf[ String ])
      //println("KRYO SOME in USE! READ:" + someClass)
      val o = kryo.readObject(input, Class.forName(someClass))
      //println("DESER KRYO SOME NON  in USE! READ:"+ o)
      return Some(o.asInstanceOf[ AnyRef ])
    }
  }

  private def cacheSomeValue(obj: Some[ AnyRef ]): Class[ _ ] =
  {
    if ( obj == null ) return null
    val claz = obj.get.getClass
    return claz
    //    if ( obj.get.isInstanceOf[ scala.Enumeration#Value ] )
    //      claz = classOf[ scala.Enumeration#Value ]
    //    // println(" is Value:" + obj.get.getClass)

  }


}

object SomeSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL_NOT_ENUM: Byte = 1;
  val NOT_NULL_ENUM: Byte = 2;

  def FITS_TO: Class[ Some[ AnyRef ] ] = classOf[ Some[ AnyRef ] ]

  def removeClassFromClassNameString(className: String): String =
  {
    className.replace("class ", "")
  }
}


