package com.protegra_ati.agentservices.core.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class SystemData[T<:Data](val data:T) extends Serializable with KVDBSerializable with UseKryoSerialization{

   def this() = this (null.asInstanceOf[T])

   private val serialVersionUID = 7526471155622776147L;

   def toStoreKey : String = {
     "systemData(" + data.toStoreKey + ")"
  }

 def toSearchKey: String =
  {
    "systemData(" + data.toSearchKey + ")"
  }


  //important to note that toStoreKey (namely formattedClassName) matches up with DisclosedData.dataDisplayClassName
//  def toDeleteKey : String = {
//    "systemData(" + data.toStoreKey.toCamelCase + ")"
//  }

//  override def equals( o : Any ) : Boolean = {
//    o match {
//      case that : SystemData[T]=> {
//        (  that.data.equals(this.data)  )
//      }
//      case _ => false
//    }
//  }
//  override def hashCode( ) : Int = {
//         ( data.hashCode()   )
//  }

}