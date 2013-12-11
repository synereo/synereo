// -*- mode: Scala;-*- 
// Filename:    AgentCnxn.scala 
// Authors:     lgm                                                    
// Creation:    Sun Apr  3 14:33:40 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.collection.mutable.HashMap

import java.io.Serializable
import java.net.URI
import java.util.UUID

trait AgentCnxnTypes extends Serializable {
  object credentialVendor extends UUIDOps with Serializable

  case class AgentCnxn(
    override val src : URI,
    override val label : String,
    override val trgt : URI
  ) extends Cnxn[URI,String,URI] {
    lazy val credential = credentialVendor.getUUID

    // Note that since a + b == b + a and 
    // (on JVM1) <URI>.hashCode == (on JVM2) <URI>.hashCode
    //
    // AgentCnxn( s, l, t )._symmetricIdentity 
    // == 
    // AgentCnxn( t, l, a )._symmetricIdentity
    // 

    def code( uri : URI ) : scala.math.BigInt = {
      new scala.math.BigInt(
        new java.math.BigInteger(
          uri.hashCode.abs.toString
        )
      )
    }

    lazy val _symmIdCode : scala.math.BigInt = {
      ( code( src ) + code( trgt ) )
    }
      
    def symmetricIdentityIntegral : scala.math.BigInt = {
      _symmIdCode
    }
    def symmetricIdentityString : String = {
      label + "_" + symmetricIdentityIntegral + ""
    }    
  }

  case class AgentBiCnxn( readCnxn : AgentCnxn, writeCnxn : AgentCnxn )

  def protoAgentCnxn : AgentCnxn = {
    AgentCnxn(
      "proto".toURI,
      "proto",
      "proto".toURI
    )
  }
}

trait AgentCnxnTypeScope {
  type ACTypes <: AgentCnxnTypes
  def protoAgentCnxnTypes : ACTypes
  val acT : ACTypes = protoAgentCnxnTypes
}
