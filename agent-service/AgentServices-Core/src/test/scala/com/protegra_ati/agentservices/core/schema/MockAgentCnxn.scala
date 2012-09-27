package com.protegra_ati.agentservices.core.schema


import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.collection.mutable.HashMap

import java.io.Serializable
import java.net.URI
import java.util.UUID
import com.biosimilarity.lift.lib.UUIDOps
import com.biosimilarity.lift.model.store.Cnxn
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization


object credentialVendor extends UUIDOps with Serializable

case class MockAgentCnxn(
  override val src: URI,
  override val label: String,
  override val trgt: URI //,
  //val credential: UUID
  ) extends Cnxn[ URI, String, URI ] with UseKryoSerialization
{
  def this() = this(null, null, null) //, credentialVendor.getUUID)


  //lazy
  val credential = credentialVendor.getUUID

  // Note that since a + b == b + a and
  // (on JVM1) <URI>.hashCode == (on JVM2) <URI>.hashCode
  //
  // AgentCnxn( s, l, t )._symmetricIdentity
  // ==
  // AgentCnxn( t, l, a )._symmetricIdentity
  //

  def code(uri: URI): scala.math.BigInt =
  {
    new scala.math.BigInt(
      new java.math.BigInteger(
        uri.hashCode.abs.toString
      )
    )
  }

  def _symmIdCode: scala.math.BigInt =
  {
    ( code(src) + code(trgt) )
  }

  def symmetricIdentityIntegral: scala.math.BigInt =
  {
    _symmIdCode
  }

  def symmetricIdentityString: String =
  {
    label + "_" + symmetricIdentityIntegral + ""
  }

  // be carefull by changing toString, see equals
  override def toString(): String =
  {
    "MockAgentCnxn[ src=" + src + ", label=" + label + ", trgt=" + trgt + ", credential=" + credential + "]"
  }


  override def hashCode =
  {41 + ( if ( src != null ) src.hashCode else 0 ) + ( if ( label != null ) label.hashCode else 0 ) + ( if ( trgt != null ) trgt.hashCode else 0 )}


  // be carefull by changing toString
  override def equals(other: Any) =
  {
    other match {
      case that: MockAgentCnxn => {
        if ( this eq that ) true
        if ( null == that ) false
        else {
          ( that canEqual this ) && (
            this.toString == that.toString

            // implicit null check
            //            ( ( ( this.src eq that.src ) && ( that.src == null ) ) || ( ( this.src != null && that.src != null ) && this.src.equals(that.src) ) ) &&
            //
            //              ( ( ( this.label eq that.label ) && ( that.label == null ) ) || ( ( this.label != null && that.label != null ) && this.label.equals(that.label) ) ) &&
            //
            //              ( ( ( this.trgt eq that.trgt ) && ( that.trgt == null ) ) || ( ( this.trgt != null && that.trgt != null ) && this.trgt.equals(that.trgt) ) ) &&
            //
            //              ( ( ( this.credential eq that.credential ) && ( that.credential == null ) ) || ( ( this.credential != null && that.credential != null ) && this.credential.toString.equals(that.credential.toString) ) )
            //            )

            )
        }
      }
      case _ =>
        false
    }
  }

  override def canEqual(other: Any) =
    other.isInstanceOf[ MockAgentCnxn ]

}



