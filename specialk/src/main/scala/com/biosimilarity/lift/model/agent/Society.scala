// -*- mode: Scala;-*- 
// Filename:    Society.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 22 23:47:36 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.agent

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.zipper._
import com.biosimilarity.lift.model.msg._

import java.net.URI
import java.util.UUID

import scala.collection.Map
import scala.collection.MapProxy
import scala.collection.mutable.{ HashMap => MHashMap, ListBuffer }
import scala.collection.immutable.{ HashMap => IHashMap }

trait SociallyZipped[ReqBody,RspBody,+SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]]
 extends Tree[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])]
  with MapProxy[Moniker,SZ[ReqBody,RspBody]] {
    def name : Moniker
    def requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]]
    def responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]]
    def collate : ( Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]] ) =
      ( name, requests, responses )
}

class Individual[ReqBody,RspBody,+SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]](
  override val name : Moniker,
  @transient
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  @transient
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]]
) extends TreeItem[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])](
  ( name, requests, responses )
) with SociallyZipped[ReqBody,RspBody,SZ] with Serializable {
  override val self = new IHashMap[Moniker,SZ[ReqBody,RspBody]]()    
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : Individual[ReqBody,RspBody,SZ] => {
	(
	  name.equals( that.name )
	  && requests.equals( that.requests )
	  && responses.equals( that.responses )
	)
      }
      case _ => {
	false
      }
    }
  }
  override def hashCode( ) : Int = {
    (
      37 * name.hashCode
      + 37 * requests.hashCode
      + 37 * responses.hashCode 
    )
  }
}

object Individual {
  def apply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    name : Moniker,
    requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
    responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]]
  ) : Individual[ReqBody,RspBody,SZ] = {
    new Individual[ReqBody,RspBody,SZ]( name, requests, responses )
  }
  def unapply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    sl : Individual[ReqBody,RspBody,SZ]
  ) : Option[( Moniker, ListBuffer[JustifiedRequest[ReqBody,RspBody]], ListBuffer[JustifiedResponse[ReqBody,RspBody]] )] = {
    Some( ( sl.name, sl.requests, sl.responses ) )
  }
}

class Society[ReqBody,RspBody,+SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]](
  val individuality : Individual[ReqBody,RspBody,SZ],
  val nameSpace : Map[Moniker,SZ[ReqBody,RspBody]]
) extends TreeSection[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])](
  nameSpace.values.toList.map(
    ( sz ) => sz.asInstanceOf[Tree[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])]]
  )
) with SociallyZipped[ReqBody,RspBody,SZ] {  
  override def name : Moniker = individuality.name
  override def requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]] =
    individuality.requests
  override def responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]] =
    individuality.responses
  override def collate : ( Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]] ) =
    individuality.collate
  override def self = nameSpace
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : Society[ReqBody,RspBody,SZ] => {
	(
	  individuality.equals( that.individuality )
	  && nameSpace.equals( that.nameSpace )
	)
      }
      case _ => {
	false
      }
    }
  }
  override def hashCode( ) : Int = {
    (
      37 * individuality.hashCode
      + 37 * nameSpace.hashCode
    )
  }
}

object Society {
  def apply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    individuality : Individual[ReqBody,RspBody,SZ],
    nameSpace : Map[Moniker,SZ[ReqBody,RspBody]]
  ) : Society[ReqBody,RspBody,SZ] = {
    new Society[ReqBody,RspBody,SZ]( individuality, nameSpace )
  }
  def unapply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    sl : Society[ReqBody,RspBody,SZ]
  ) : Option[( Individual[ReqBody,RspBody,SZ], Map[Moniker,SZ[ReqBody,RspBody]] )] = {
    Some( ( sl.individuality, sl.nameSpace ) )
  }
}

class RemoteSociety[ReqBody,RspBody,+SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]](
  val individuality : Individual[ReqBody,RspBody,SZ],
  val acquaintances : List[Moniker]
) extends TreeSection[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])](
  acquaintances.map(
    ( acq ) => {
      Individual(
	acq,
	new ListBuffer[JustifiedRequest[ReqBody,RspBody]]( ),
	new ListBuffer[JustifiedResponse[ReqBody,RspBody]]( )
      ).asInstanceOf[Tree[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]])]]
    }
  )
) with SociallyZipped[ReqBody,RspBody,SZ] {  
  override def name : Moniker = individuality.name
  override def requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]] =
    individuality.requests
  override def responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]] =
    individuality.responses
  override def collate : ( Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]] ) =
    individuality.collate
  override val self = new IHashMap[Moniker,SZ[ReqBody,RspBody]]()
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : RemoteSociety[ReqBody,RspBody,SZ] => {
	(
	  individuality.equals( that.individuality )
	  && acquaintances.equals( that.acquaintances )
	)
      }
      case _ => {
	false
      }
    }
  }
  override def hashCode( ) : Int = {
    (
      37 * individuality.hashCode
      + 37 * acquaintances.hashCode
    )
  }
}

object RemoteSociety {
  def apply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    individuality : Individual[ReqBody,RspBody,SZ],
    acquaintances : List[Moniker]
  ) : RemoteSociety[ReqBody,RspBody,SZ] = {
    new RemoteSociety[ReqBody,RspBody,SZ]( individuality, acquaintances )
  }
  def unapply [ReqBody,RspBody,SZ[Rq <: ReqBody, Rs <: RspBody] <: SociallyZipped[Rq,Rs,SZ]] (
    sl : RemoteSociety[ReqBody,RspBody,SZ]
  ) : Option[( Individual[ReqBody,RspBody,SZ], List[Moniker] )] = {
    Some( ( sl.individuality, sl.acquaintances ) )
  }
}

