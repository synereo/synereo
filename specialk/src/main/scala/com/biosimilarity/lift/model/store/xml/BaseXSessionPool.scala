// -*- mode: Scala;-*- 
// Filename:    BaseXSessionPool.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 11 05:26:32 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store.SessionPool

import org.basex.server.ClientSession

object BaseXSessionPool extends SessionPool[ClientSession] {
  case class BaseXClientSessionFactory(
    override val host : String, override val port : Int,
    override val user : String, override val pwd : String
  ) extends PoolableClientSessionFactory( host, port, user, pwd ) {
    override def sessionFromConfig: ClientSession = {
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
