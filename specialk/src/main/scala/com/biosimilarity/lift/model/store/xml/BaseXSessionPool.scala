// -*- mode: Scala;-*- 
// Filename:    BaseXSessionPool.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 11 05:26:32 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store.SessionPool
import com.biosimilarity.lift.model.store.URIFromConfigurationT

import org.basex.server.ClientSession

import java.net.URI

class BaseXSessionPoolC(
  val maxObjsInPool : Int,
  val maxCreations : Int,
  @transient
  implicit override val configInfo : URIFromConfigurationT
) extends SessionPool[ClientSession](
  maxObjsInPool, maxCreations, configInfo
){  
  case class BaseXClientSessionFactory(
    override val host : String, override val port : Int,
    override val user : String, override val pwd : String
  ) extends PoolableClientSessionFactory( host, port, user, pwd ) {
    override def getSession : ClientSession = {
      new ClientSession( host, port, user, pwd )
    }
  }

  override def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory = {
    BaseXClientSessionFactory( host, port, user, pwd )
  }
}

object BaseXSessionPool extends BaseXSessionPoolC( 2000, 65, BaseXConfigInfo )
