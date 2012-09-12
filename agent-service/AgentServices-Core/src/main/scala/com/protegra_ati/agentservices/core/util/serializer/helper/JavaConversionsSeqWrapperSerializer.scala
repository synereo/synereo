package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema.Post

/* User: mgevantmakher
*/

class JavaConversionsSeqWrapperSerializer() extends Serializer[ SeqWrapper[ Any ] ]
{
  var elementsCanBeNull = true
  var serializer: Serializer[ Any ] = null
  var elementClass: Class[ Any ] = null
  var length: Int = 0

  locally {
    // setImmutable(true)
    setAcceptsNull(true)
  }

  def setElementsCanBeNull(_elementsCanBeNull: Boolean) =
  {
    elementsCanBeNull = _elementsCanBeNull
  }

  /**Sets the number of objects in the collection. Saves 1-2 bytes. */
  def setLength(_length: Int) =
  {
    length = _length
  }

  def setElementClass(_elementClass: Class[ Any ], _serializer: Serializer[ Any ]) =
  {
    elementClass = _elementClass
    serializer = _serializer
  }

  //  //override
  //  def create(kryo: Kryo, input: Input, typ: Class[ Traversable[ _ ] ]): Traversable[ _ ] =
  //  {
  //    val len = if ( length != 0 ) length else input.readInt(true)
  //    val inst = kryo.newInstance(typ)
  //    val coll = inst.asInstanceOf[ Traversable[ Any ] ].genericBuilder[ Any ]
  //    if ( len != 0 ) {
  //      if ( serializer != null ) {
  //        if ( elementsCanBeNull ) {
  //          0 until len foreach {_ => coll += kryo.readObjectOrNull(input, elementClass, serializer)}
  //        } else {
  //          0 until len foreach {_ => coll += kryo.readObject(input, elementClass, serializer)}
  //        }
  //      } else {
  //        0 until len foreach {_ => coll += kryo.readClassAndObject(input)}
  //      }
  //    }
  //
  //    coll.result
  //  }


  override def read(kryo: Kryo, input: Input, typ: Class[ SeqWrapper[ Any ] ]): SeqWrapper[ Any ] =
  {
    System.err.println("READ JavaConversionsSeqWrapperSerializer")
    System.err.println("READ START")
    val l: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( l == JavaConversionsSeqWrapperSerializer.NULL ) return null

    val len = if ( length != 0 ) length else input.readInt(true)
    // val inst = kryo.newInstance(typ)
    var coll: List[ Any ] = Nil
    System.err.println("READ COLL SIZE:" + len)
    //val coll = inst.asInstanceOf[ SeqWrapper[ Any ] ].genericBuilder[ Any ]
    //inst.asInstanceOf[ Traversable[ Any ] ].genericBuilder[ Any ]
    if ( len != 0 ) {
      if ( serializer != null ) {
        if ( elementsCanBeNull ) {
          0 until len foreach {
            _ => {
              val s = kryo.readObjectOrNull(input, elementClass, serializer)
              coll = s :: coll
            }
          }
        } else {
          0 until len foreach {
            _ => {
              val s = kryo.readObject(input, elementClass, serializer)
              coll = s :: coll
            }
          }
        }
      } else {
        0 until len foreach {
          _ => {
            val s = kryo.readClassAndObject(input)
            coll = s :: coll
          }
        }
      }
    }
    coll = coll.reverse
    //System.err.println("READ DONE:" + coll)
    val javaColl: java.util.List[ Any ] = coll
    //System.err.println("READ DONE:" + javaColl)
    return javaColl.asInstanceOf[ SeqWrapper[ Any ] ]
  }

  override def write(kryo: Kryo, output: Output, obj: SeqWrapper[ Any ]) =
  {

    System.err.println("WRITE JavaConversionsSeqWrapperSerializer")
    //System.err.println("WRITE START")
    if ( obj == null ) {
      kryo.writeObject(output, JavaConversionsSeqWrapperSerializer.NULL)
    }
    else {
      if ( obj == Nil ) System.err.println("-----------------NILLLLLLLLLLLLLLLLLLLLLLLLLLLLLL")
      kryo.writeObject(output, JavaConversionsSeqWrapperSerializer.NOT_NULL)
      val collection: Traversable[ Any ] = obj
      val len = if ( length != 0 ) length
      else {
        val size = collection.size
        output.writeInt(size, true)
      }

      if ( len != 0 ) {
        if ( serializer != null ) {
          if ( elementsCanBeNull ) {
            collection.foreach {element => kryo.writeObjectOrNull(output, element, serializer)}
          } else {
            collection.foreach {element => kryo.writeObject(output, element, serializer)}
          }
        } else {
          collection.foreach {element => kryo.writeClassAndObject(output, element)}
        }
      }
      // System.err.println("WRITE DONE")
    }
  }
}

object JavaConversionsSeqWrapperSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ SeqWrapper[ Any ] ] = classOf[ SeqWrapper[ Any ] ]
}

